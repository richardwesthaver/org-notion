;;; org-notion.el --- Notion.so Org-mode extension  -*- lexical-binding: t; -*-
;; 
;; Copyright (C) 2021  ellis
;; 
;; Author: ellis <ellis@rwest.io>
;; Version: 0.1.0
;; Keywords: wp, extensions
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;; Commentary:
;; 
;; 
;;; Code:
(require 'auth-source)

(defconst org-notion-host "api.notion.com"
  "FQDN of Notion API. This is used to create an entry with
`auth-source-secrets-create'.")

(defconst org-notion-endpoint (format "https://%s/v1/" org-notion-host)
  "URI of Notion API endpoint")

(defconst org-notion-version "2021-08-16"
  "Notion API Version")

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

(defcustom org-notion-pages (list org-directory)
  "List of directories to sync with Notion.so counterparts."
  :type 'list
  :group 'org-notion)

(defcustom org-notion-push-hook nil
  "Hook to run after `org-notion-push'"
  :type 'hook
  :group 'org-notion)

(defcustom org-notion-pull-hook nil
  "Hook to run after `org-notion-pull'"
  :type 'hook
  :group 'org-notion)

(defvar org-notion--block-types
  (list "paragraph"
	"heading_1"
	"heading_2"
	"heading_3"
	"bulleted_list_item"
	"numbered_list_item"
	"to_do"
	"toggle"
	"child_page"
	"child_database"
	"embed"
	"image"
	"video"
	"file"
	"pdf"
	"bookmark"
	"callout"
	"quote"
	"equation"
	"divider"
	"table_of_contents"
	"column"
	"column_list"
	"link_preview"
	"synced_block"
	"template"
	"link_to_page"
	"unsupported")
  "Type of blocks available via Notion API, used by
`org-notion-block' object. 'unsupported' refers to an unsupported
block type.")

;;; Errors
(define-error 'org-notion-err nil)
(define-error 'org-notion-err--api nil 'org-notion-err)
(define-error 'org-notion--not-found "Resource not found" 'org-notion-err--api)
(define-error 'org-notion--rate-limit "Req-limit reached -- slow down" 'org-notion-err--api)
(define-error 'org-notion--ser-err "Encountered error during serialization" 'org-notion-err)
(define-error 'org-notion--de-err "Encountered error during deserialization" 'org-notion-err)
(define-error 'org-notion--auth-err "Encountered error during authentication" 'org-notion-err)

;;; Notion Objects 
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
	   :documentation "The parent of this page. Can be a page or workspace.")
   (url :initarg :url
	:documentation "The URL of the Notion database."))
  "Notion.so database object - identified by the `:id' slot.")

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

;;; Notion API calls  
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
		    (switch-to-buffer (current-buffer))))))

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
  (interactive)
  (let ((query query)
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-token)))
	   ("Notion-Version" . ,org-notion-version)))
	(url-request-data (json-encode (list :query query))))
    (url-retrieve (concat org-notion-endpoint "search")
		  (lambda (_status)
		    (with-current-buffer (current-buffer) (message "success"))))))

(provide 'org-notion)
;;; org-notion.el ends here
