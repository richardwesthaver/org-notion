;;; org-notion.el --- Notion.so + Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  ellis

;; Author: ellis <ellis@rwest.io>
;; Version: 0.1.0
;; Keywords: wp, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-notion is a simple package that integrates the beauty and
;; convenience of Notion.so with your Org-mode workflow. 
;; 
;; Many moons ago I used Notion.so as a notes and collaboration tool
;; for work. It's easy to use and is (in my opinion) the best
;; web-based solution out there. I have always used Org-mode for
;; personal ventures of course, but it is a pain to integrate with
;; non-tech folks. Even tech folks will roll their eyes if your
;; company-hosted repo has a README.org instead of the tried and true
;; README.md. I decided to abandon Notion.so because I couldn't wait
;; for the overdue official API to be released, and was unsatisfied
;; with the unofficial variants. Nowadays, that story has changed. As
;; of 2021 the official API is available and ready for action.
;; 
;; This package wraps the full Notion.so API with support for all web
;; requests and object types. The requests are dispatched
;; asynchronously via the `org-notion-call' function which returns an
;; EIEIO class instance based on the CALLBACK arg. This is the
;; low-level interface which can be hacked to your liking.
;;
;; The Org-mode integration is achieved with the help of the
;; `org-element' functions which translate Org syntax to Notion.so
;; rich text objects and vice-versa. An `org-notion-mode' minor-mode
;; is provided for interacting with the API from auto-loaded functions
;; and custom properties are used to keep headlines/files in sync with
;; their Notion.so counterparts.

;;; Code:
(require 'eieio)
(require 'auth-source)
(require 'json)
(require 'url)

(defconst org-notion-host "api.notion.com"
  "FQDN of Notion API. This is used to create an entry with
`auth-source-secrets-create'.")

(defconst org-notion-endpoint (format "https://%s/v1/" org-notion-host)
  "URI of Notion API endpoint")

(defconst org-notion-version "2021-08-16"
  "Notion API Version")

(defconst org-notion--block-types '(paragraph heading_1 heading_2
  heading_3 bulleted_list_item numbered_list_item to_do toggle
  child_page child_database embed image video file pdf bookmark
  callout quote equation divider table_of_contents column
  column_list link_preview synced_block template link_to_page
  unsupported)
  "Type of blocks available for Notion API, used by
`org-notion-block' object. 'unsupported' refers to an unsupported
block type.")

(defconst org-notion--annotation-types '(bold italic strikethrough
  underline code)
  "Annotations available for Notion API text objects, used by
`org-notion-rich-text' object.")

(defconst org-notion--color-types '(default gray brown orange
  yellow green blue purple pink red gray_background
  brown_background orange_background yellow_background
  green_background blue_background purple_background
  pink_background red_background)
  "Colors available for Notion API text objects, used by
`org-notion-rich-text' object.")

(defgroup org-notion nil
  "Customization group for org-notion."
  :tag "Org Notion"
  :group 'org)

(defcustom org-notion-use-auth-source t
  "Check auth-source for Notion integration token. Falls back to
interactive prompt if token isn't found."
  :type 'boolean
  :group 'org-notion)

(defcustom org-notion-page-size 100
  "Default count of results returned by Notion. Additional calls
will be made to collect all results using the 'start_cursor'
parameter. Maximum value is 100."
  :type 'integer
  :group 'org-notion)

(defcustom org-notion-coding-system 'utf-8
  "Use custom coding system for org-notion."
  :group 'org-notion)

(defcustom org-notion-push-hook nil
  "Hook to run after `org-notion-push'"
  :type 'hook
  :group 'org-notion)

(defcustom org-notion-pull-hook nil
  "Hook to run after `org-notion-pull'"
  :type 'hook
  :group 'org-notion)

(defvar org-notion-id-property "NOTION_ID"
  "Name of NOTION_ID Org-mode property.")

(defvar org-notion-page-list nil
  "List of Notion.so pages.")

(defvar org-notion-database-list nil
  "List of Notion.so databases.")

(defvar org-notion-buffer-name "*org-notion-%s*"
  "Name of the org-notion buffer.")

(defvar org-notion-buffer-kill-prompt t
  "Ask before killing org-notion buffer.")

(make-variable-buffer-local 'org-notion-buffer-kill-prompt)

;;; Errors
(defvar org-notion-verbosity 'debug)
(defun org-notion-log (s)
  (when (eq 'debug org-notion-verbosity) (message "%s" s)))

;;; EIEIO
(defclass org-notion-object ()
  ((id :initarg :id :type string :required t
       :documentation "UUID v4 associated with this object")
   (object :initarg :object :type string :required t
	   :documentation "one of: 'page', 'database', 'block',
	   or 'user'"))
  "Top-level class for Notion API objects.")

(defclass org-notion-user (org-notion-object)
  ((type :initarg :type :type string
	 :documentation "Type of the user. This slot should be
either 'person' or 'bot'.")
   (name :initarg :name :type string
	 :documentation "User's name, as displayed in Notion.")
   (avatar :initarg :avatar :type string
	   :documentation "Chosen avatar image.")
   (email :initarg :email :type (or null string)
	  :documentation "Email address of a user. Only present
if `:type' is 'person' and integration has user capabilities that
allow access to email addresses.")
   (owner-type :initarg :owner-type :type (or nil string)
	       :documentation "The type of owner -- either
	       'workspace' or 'user'")
   (owner :initarg :owner :type (or null object)
	  :documentation "The owner of a bot user -- either
	  nil, indicating 'workspace' is owner, or an
	  `org-notion-user' object of `:type' 'person'."))
  "Notion.so user object - can be a real person or a
bot. Identified by the `:id' slot.")

(defclass org-notion-block (org-notion-object)
  ((type :initarg :type :type string
	 :documentation "Type of block. See variable
	 `org-notion--block-types' for possible values.")
   (created :initarg :created
	    :documentation "Datetime when this block was
	    created.")
   (updated :initarg :updated
	    :documentation "Datetime when this block was last
	    updated.")
   (archived :initarg :archived :type boolean
	     :documentation "The archived status of the block.")
   (has_children :initarg :has_children :type boolean
		 :documentation "Whether or not the block has
		 children blocks nested within it."))
  "Notion.so block object - identified by the `:id' slot.")

(defclass org-notion-page (org-notion-object)
  ((created :initarg :created
	    :documentation "Datetime when this page was
	    created.")
   (updated :initarg :updated
	    :documentation "Datetime when this page was
	    updated.")
   (archived :initarg :archived
	     :documentation "The archived status of the page.")
   (icon :initarg :icon
	 :documentation "Page icon.")
   (cover :initarg :cover
	  :documentation "Page cover image.")
   (properties :initarg :properties
	       :documentation "Property values of this page.")
   (parent :initarg :parent
	   :documentation "The parent of this page. Can be a
	   database, page, or workspace.")
   (url :initarg :url
	:documentation "The URL of the Notion page."))
  "Notion.so page object - identified by the `:id' slot.")

(defclass org-notion-database (org-notion-object)
  ((created :initarg :created
	    :documentation "Datetime when this database was
	    created.")
   (updated :initarg :updated
	    :documentation "Datetime when this database was
	    updated.")
   (title :initarg :title
	  :documentation "Name of the database as it appears in
	  Notion.")
   (icon :initarg :icon
	 :documentation "Page icon.")
   (cover :initarg :cover
	  :documentation "Page cover image.")
   (properties :initarg :properties
	       :documentation "Schema of properties for the
	       database as they appear in Notion.")
   (parent :initarg :parent
	   :documentation "The parent of this page. Can be a page
	   or workspace.")
   (url :initarg :url
	:documentation "The URL of the Notion database."))
  "Notion.so database object - identified by the `:id' slot.")

(defclass org-notion-rich-text ()
  ((type :initarg :type :type string :required t
	 :documentation "Type of this rich text object. Possible
	 values are: 'text', 'mention', 'equation'")
   (plain_text :initarg :plain_text :type string :required t
	       :documentation "The plain text without
	       annotations.")
   (href :initarg :href :type string
	 :documentation "The URL of any link or internal Notion
	 mention in this text, if any.")
   (annotations :initarg :annotations
		:documentation "All annotations that apply to
		this rich text. See
		`org-notion--annotation-types' for a list of
		possible values.")
   (color :initarg :color
	  :documentation "Color that applies to this rich
	  text. See `org-notion--color-types' for a list of
	  possible values."))
  "Notion.so rich text object.")

(defclass org-notion-inline-text (org-notion-rich-text)
  ((content :initarg :content :type string :required t
	    :documentation "Text content.")
   (link :initarg :link :type string
	 :documentation "Any inline link in this text."))
  "Notion.so inline text object found in `org-notion-rich-text'
of type 'text'")

(defclass org-notion-inline-mention (org-notion-rich-text)
  ((mention_type :initarg :mention_type :type string :required t
		 :documentation "Type of the inline mention. See
		 `org-notion--mention-types' for a list of
		 possible values."))
  "Notion.so inline mention object found in
`org-notion-rich-text' of type 'mention'")

(defclass org-notion-inline-equation (org-notion-rich-text)
  ((expression :initarg :expression :type string :required t
	       :documentation "The LaTeX string representing this inline equation."))
  "Notion.so inline equation object found in `org-notion-rich-text' of type 'equation'.")

(defclass org-notion-request ()
  ((token :initform (org-notion-token) :initarg :token :required t :allocation :class
	  :documentation "Bearer token used to authenticate requests.")
   (version :initform org-notion-version :initarg :version :required t :allocation :class
	    :documentation "Notion.so API Version.")
   (endpoint :initform org-notion-endpoint :inittarg :endpoint :required t :allocation :class
	     :documentation "Notion.so API endpoint.")
   (data :initarg :data
	 :documentation "Payload to be sent with HTTP request."))
  "Notion.so API request.")

;;; Authentication
;; RESEARCH 2021-12-28: auth-source secret const function security reccs
(defun org-notion-token (&optional token)
  "Find the Notion.so API Integration Token.
If `org-notion-use-auth-source' is t check auth-source first. If
nil or TOKEN missing, prompt for token.  You can generate a new
token at URL `https://www.notion.so/my-integrations'."
  (interactive)
  (if org-notion-use-auth-source
      (let ((auth-source-creation-defaults '((user . "org-notion")
					     (port . "443")))
	    (found (nth 0 (auth-source-search
			   :host org-notion-host
			   :max 1
			   :require '(:secret)
			   :create t))))
	(when found
	  (let ((sec (plist-get found :secret)))
	    (if (functionp sec)
		(funcall sec)
	      sec))))
    (or token
	(read-passwd "Notion API Token: "))))

;;; Helpers
(defun org-notion-to-org-time (iso-time-str)
  "Convert ISO-TIME-STR to format \"%Y-%m-%d %T\".
Example: \"2012-01-09T08:59:15.000Z\" becomes \"2012-01-09
03:59:15\", with the current timezone being -0500."
  (condition-case _
      (org-format-time-string
       "%Y-%m-%d %T"
       (apply
        'encode-time
        (iso8601-parse iso-time-str)))
    (error iso-time-str)))

;; TODO 2022-01-07: account for timezone
(defun org-notion-from-org-time (org-time-str)
  "Convert ORG-TIME-STR back to ISO-8601 time format."
  (condition-case _
      (format-time-string
       "%FT%T%z"
       (apply 'encode-time
	      (parse-time-string org-time-str))
       t)
    (error org-time-str)))

;;; API Requests
(defun org-notion--json-read ()
  "Read with json"
  (decode-coding-region (point) (point-max) org-notion-coding-system)
  (json-read))

(defun org-notion-call (method callback &rest params)
  "Call the Notion.so API with METHOD, then CALLBACK with supplied PARAMS.")
(defun org-notion-get-current-user ()
  "Retrieve the bot user associated with the current
`org-notion-token'"
  (interactive)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-token)))
	   ("Notion-Version" . ,org-notion-version))))
    (url-retrieve (concat org-notion-endpoint "users/me")
		  (lambda (_status)
		    (with-current-buffer (current-buffer)
		    (search-forward "\n\n")
		    (message "%s" (alist-get 'id (json-read))))))))

(defun org-notion-get-users ()
  "Get all users in the Notion workspace. This will return a 403
status code if your integration doesn't have User Capabilities
enabled."
  (interactive)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-token)))
	   ("Notion-Version" . ,org-notion-version))))
    (url-retrieve (concat org-notion-endpoint "users")
		  (lambda (_status)
		    (switch-to-buffer (current-buffer))))))

(defun org-notion-search (query)
  "Search the Notion workspace using QUERY"
  (interactive
   "squery: ")
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-token)))
	   ("Notion-Version" . ,org-notion-version)))
	(url-request-data (json-encode (list :query query))))
    (url-retrieve (concat org-notion-endpoint "search")
		  (lambda (_status)
		    (with-current-buffer (current-buffer) (message "success"))))))

;;; Org-mode
(defun org-notion-id-at-point (&optional pom)
  "Get the value of `org-notion-id-property' key closest to POM."
  (or (cdr (assoc org-notion-id-property (org-entry-properties)))
      (cadr (assoc org-notion-id-property (org-collect-keywords `(,org-notion-id-property))))))

;;;###autoload
(defun org-notion-open ()
  "Open the Notion.so page attached to this heading or file in
browser. Requires `org-notion-id-property' key to be set."
  (interactive)
  (let ((notion-id (org-notion-id-at-point)))
    (if notion-id (browse-url (format "https://www.notion.so/%s" notion-id))
      (message "failed to find %s property" org-notion-id-property))))

(provide 'org-notion)
;;; org-notion.el ends here
