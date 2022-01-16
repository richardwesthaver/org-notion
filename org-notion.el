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

;;; Code:
(require 'eieio-base)
(require 'auth-source)
(require 'json)
(require 'url)

(defconst org-notion-host "api.notion.com"
  "FQDN of Notion API. This is used to create an entry with
`auth-source-secrets-create'.")

(defconst org-notion-version "2021-08-16"
  "Notion API Version")

(defconst org-notion-block-types '(paragraph heading_1 heading_2
  heading_3 bulleted_list_item numbered_list_item to_do toggle
  child_page child_database embed image video file pdf bookmark
  callout quote equation divider table_of_contents column
  column_list link_preview synced_block template link_to_page
  unsupported)
  "Type of blocks available for Notion API, used by
`org-notion-block' class. 'unsupported' refers to an unsupported
block type.")

(defconst org-notion-annotation-types '(bold italic strikethrough
  underline code)
  "Annotations available for Notion API text objects, used by
`org-notion-rich-text' class.")

(defconst org-notion-color-types '(default gray brown orange
  yellow green blue purple pink red gray_background
  brown_background orange_background yellow_background
  green_background blue_background purple_background
  pink_background red_background)
  "Colors available for Notion API text objects, used by
`org-notion-rich-text' class.")

(defconst org-notion-mention-types '(user page database date link_preview)
  "Mention types available for Notion API text objects, used by
  `org-notion-rich-text' class.")

(defconst org-notion-method-types '(search
 current-user user users 
 database query-database create-database update-database
 page page-property create-page update-page
 block block-children update-block append-block delete-block)
  "Method types available for Notion API requests, used by
`org-notion-request' class.")

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

(defvar org-notion-endpoint (format "https://%s/v1/" org-notion-host)
  "URI of Notion API endpoint")

(defvar org-notion-id-property "NOTION_ID"
  "Name of NOTION_ID Org-mode property.")

(defvar org-notion-uuid-regexp "\\<[[:xdigit:]]\\{8\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{12\\}\\>"
  "A regular expression matching a UUID with or without hyphens.")

(defvar org-notion-page-cache nil
  "Hashtable of Notion pages.")

(defvar org-notion-database-cache nil
  "Hashtable of Notion databases.")

(defvar org-notion-block-cache nil
  "Hashtable of Notion blocks.")

(defvar org-notion-object-tracker nil
  "List of `org-notion-object' instances.")

(defvar org-notion-proc-buffer "org-notion-proc"
  "Name of the org-notion process buffer.")

(defvar org-notion-buffer-kill-prompt t
  "Ask before killing org-notion buffer.")

(make-variable-buffer-local 'org-notion-buffer-kill-prompt)

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "notion"
			     :follow 'org-notion-browse
			     :complete (lambda ()
					 (format "notion:%s"))))
    
;;; Errors
(defvar org-notion-verbosity 'debug)

(defun org-notion-log (s)
  (when (eq 'debug org-notion-verbosity) (message "%s" s)))

(define-error 'org-notion-error "Unknown org-notion error")

(define-error 'org-notion-invalid-uuid "String is not a valid UUID" 'org-notion-error)
(define-error 'org-notion-invalid-method "Invalid API method" 'org-notion-error)

;; API Responses
(define-error 'org-notion-bad-request "HTTP 400: Bad Request" 'org-notion-error)
(define-error 'org-notion-unauthorized "HTTP 401: Unauthorized" 'org-notion-error)
(define-error 'org-notion-restricted-resource "HTTP 403: Restricted Resource" 'org-notion-error)
(define-error 'org-notion-not-found "HTTP 404: Object not found" 'org-notion-error)
(define-error 'org-notion-conflict "HTTP 409: Conflict error possibly due to data collision" 'org-notion-error)
(define-error 'org-notion-rate-limited "HTTP 429: Exceeded number of requests allowed" 'org-notion-error)
(define-error 'org-notion-unexpected "HTTP 500: Internal server error occurred" 'org-notion-error)
(define-error 'org-notion-unavailable "HTTP 503: Notion is unavailable" 'org-notion-error)
(define-error 'org-notion-database-unavailable "HTTP 503: Notion database service is unavailable" 'org-notion-error)

(defun org-notion-api-error (status)
  "Propagate an error message returned from Notion API.")

;;; EIEIO
;; Default superclass. This is inherited by all other org-notion
;; classes and should only define new static methods. The only method
;; implemented is `org-notion-dump' which simply prints the fields of
;; a class instance.
(defclass org-notion-class nil
  nil
  :documentation "Default superclass inherited by `org-notion' classes."
  :abstract "Class org-notion-class is abstract.
use `org-notion-object' `org-notion-rich-text' or `org-notion-request' to create instances.")

(cl-defmethod org-notion-dump ((obj org-notion-class))
  "Pretty-print EIEIO class objects as string."
  (let ((slots (mapcar (lambda (slot) (aref slot 1)) (eieio-class-slots (eieio-object-class obj)))))
    (setq slots (cl-remove-if (lambda (s) (not (slot-boundp obj s))) slots))
    (apply #'concat
	   (mapcar (lambda (slot)
		     (let ((slot (intern (pp-to-string slot))))
		       (format "\n%+4s:   %s" slot (slot-value obj (intern (pp-to-string slot))))))
		   slots))))

;; Parent class of Notion objects. The 'id' and 'data' properties are
;; inherited by subclasses. 'tracking-symbol' is a variable used to
;; store instances.
(defclass org-notion-object (org-notion-class eieio-instance-tracker)
  ((id
    :initform nil
    :initarg :id
    :type (or null string)
    :accessor org-notion-id
    :documentation "UUID v4 associated with this object")
   (tracking-symbol :initform 'org-notion-object-tracker))
  :documentation "Top-level class for Notion API objects.")

(cl-defmethod org-notion-id ((obj org-notion-object))
  "Return the 'id' value of OBJ, signals an error if not a valid
UUID."
  (when-let ((id (org-notion-valid-uuid (oref obj :id)))) id))

;;;; Generic Functions
;; The following generics are implemented by `org-notion-object'
;; subclasses.
(cl-defgeneric org-notion-from-json (obj json)
  "Interpret JSON as `org-notion-object'")

(cl-defgeneric org-notion-to-json (obj)
  "Interpret `org-notion-object' OBJ as json object.")

(cl-defgeneric org-notion-from-org (obj str)
  "Interrpret Org-mode STR as `org-notion-object'")

(cl-defgeneric org-notion-to-org (obj)
  "Interpret `org-notion-object' OBJ as Org-mode syntax.")

;; User object
(defclass org-notion-user (org-notion-object)
  ((type
    :initform ""
    :initarg :type
    :type string
    :documentation "Type of the user. This slot should be either
    'person' or 'bot'.")
   (name
    :initform ""
    :initarg :name
    :type string
    :documentation "User's name, as displayed in Notion.")
   (avatar
    :initform nil
    :initarg :avatar
    :type (or null string)
    :documentation "Chosen avatar image.")
   (email
    :initform nil
    :initarg :email
    :type (or null string)
    :documentation "Email address of a user. Only present if
    `:type' is 'person' and integration has user capabilities
    that allow access to email addresses.")
   (owner-type
    :initform nil
    :initarg :owner-type
    :type (or null string)
    :documentation "The type of owner -- either 'workspace' or
    'user'")
   (owner
    :initform nil
    :initarg :owner
    :type (or null object)
    :documentation "The owner of a bot user -- either null,
    indicating 'workspace' is owner, or an `org-notion-user'
    object of `:type' 'person'."))
  :documentation "Notion.so user object - can be a real person or
a bot. Identified by the `:id' slot.")

(cl-defmethod org-notion-email ((obj org-notion-user))
  "Return the 'email' value of OBJ."
  (when-let (email (oref obj :email))
    email))

(cl-defmethod org-notion-name ((obj org-notion-user))
  "Return the 'name' value of OBJ."
  (when (not (string-empty-p (oref obj :name)))
    (oref obj :name)))

(cl-defmethod org-notion-from-json ((obj org-notion-user) json))

(cl-defmethod org-notion-to-json ((obj org-notion-user)))

;; Database object
(defclass org-notion-database (org-notion-object)
  ((title
    :initform ""
    :initarg :title
    :documentation "Name of the database as it appears in
    Notion.")
   (created
    :initform ""
    :initarg :created
    :type string
    :documentation "Datetime when this database was created.")
   (updated
    :initform ""
    :initarg :updated
    :type string
    :documentation "Datetime when this database was updated.")
   (icon
    :initform nil
    :initarg :icon
    :type (or null string)
    :documentation "Page icon.")
   (cover
    :initform nil
    :initarg :cover
    :type (or null string)
    :documentation "Page cover image.")
   (properties
    :initarg :properties
    :documentation "Schema of properties for the database as they
    appear in Notion.")
   (parent
    :initform ""
    :initarg :parent
    :type (or string object)
    :documentation "The parent of this page. Can be a page or
    workspace.")
   (url
    :initform ""
    :initarg :url
    :type string
    :documentation "The URL of the Notion database."))
  :documentation "Notion.so database object - identified by the
`:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-database) json)
  ;; quick check
  (if (string= "database" (alist-get 'object json))
      (progn
	(oset obj :id (alist-get 'id json))
	(oset obj :title (cdr 		; down the rabbit-hole we go
			  (cadr
			   (cadr
			    (mapcan
			     (lambda (x)
			       (if (listp x) x nil))
			     (alist-get 'title json)))))) ; this is a vector
	(oset obj :created (alist-get 'created_time json))
	(oset obj :updated (alist-get 'last_edited_time json))
	(oset obj :icon (alist-get 'icon json))
	(oset obj :cover (alist-get 'cover json))
	(oset obj :properties (alist-get 'properties json))
	(oset obj :parent (cdar (alist-get 'parent json))) ; retrieve parent.type
	(oset obj :url (alist-get 'url json)))
    (error "expected database object, found %s" (alist-get 'object json)))
  obj)

;; Page object
(defclass org-notion-page (org-notion-object)
  ((created
    :initarg :created
    :documentation "Datetime when this page was created.")
   (updated
    :initarg :updated
    :documentation "Datetime when this page was updated.")
   (archived
    :initform nil
    :initarg :archived
    :type boolean
    :documentation "The archived status of the page.")
   (icon
    :initform nil
    :initarg :icon
    :type (or null string)
    :documentation "Page icon.")
   (cover
    :initform nil
    :initarg :cover
    :type (or null string)
    :documentation "Page cover image.")
   (properties
    :initarg :properties
    :documentation "Property values of this page.")
   (parent
    :initform nil
    :initarg :parent
    :type (or null object)
    :documentation "The parent of this page. Can be a database,
    page, or workspace.")
   (url
    :initform ""
    :initarg :url
    :type string
    :documentation "The URL of the Notion page."))
  :documentation "Notion.so page object - identified by the `:id'
slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-page) json))

(cl-defmethod org-notion-to-json ((obj org-notion-page) json))

;; Block object
(defclass org-notion-block (org-notion-object)
  ((type
    :initform unsupported
    :initarg :type
    :type symbol
    :documentation "Type of block. See variable
    `org-notion-block-types' for possible values.")
   (created
    :initarg :created
    :documentation "Datetime when this block was created.")
   (updated
    :initarg :updated
    :documentation "Datetime when this block was last updated.")
   (archived
    :initform nil
    :initarg :archived
    :type boolean
    :documentation "The archived status of the block.")
   (has_children
    :initform nil
    :initarg :has_children
    :type boolean
    :documentation "Whether or not the block has children blocks
    nested within it."))
  :documentation "Notion.so block object - identified by the
`:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-block) json))

(cl-defmethod org-notion-to-json ((obj org-notion-block) json))

;; Parent class for Notion Rich-text objects. Instances of subclasses
;; are de/serialized as an array of JSON objects for interaction with
;; the Notion API and as Org syntax.
(defclass org-notion-rich-text (org-notion-class)
  ((type
    :initform "text"
    :initarg :type
    :type string
    :documentation "Type of this rich text object. Possible
    values are: 'text', 'mention', 'equation'")
   (plain_text
    :initform ""
    :initarg :plain_text
    :type string
    :documentation "The plain text without annotations.")
   (href
    :initform nil
    :initarg :href
    :type (or null string)
    :documentation "The URL of any link or internal Notion
    mention in this text, if any.")
   (annotations
    :initform nil
    :initarg :annotations
    :type (or null string)
    :documentation "All annotations that apply to this rich
    text. See `org-notion-annotation-types' for a list of
    possible values.")
   (color
    :initform default
    :initarg :color
    :type symbol
    :documentation "Color that applies to this rich text. See
    `org-notion-color-types' for a list of possible values."))
  :documentation "Notion.so rich text object.")

(defclass org-notion-inline-text (org-notion-rich-text)
  ((content
    :initform ""
    :initarg :content
    :type string
    :documentation "Text content.")
   (link
    :initform ""
    :initarg :link
    :type (or null string)
    :documentation "Any inline link in this text."))
  :documentation "Notion.so inline text object found in
`org-notion-rich-text' of type 'text'")

(defclass org-notion-inline-mention (org-notion-rich-text)
  ((mention_type
    :initform nil
    :initarg :mention_type
    :type symbol
    :documentation "Type of the inline mention. See
    `org-notion-mention-types' for a list of possible values."))
  :documentation "Notion.so inline mention object found in
`org-notion-rich-text' of type 'mention'")

(defclass org-notion-inline-equation (org-notion-rich-text)
  ((expression
    :initform ""
    :initarg :expression
    :type string
    :documentation "The LaTeX string representing this inline
    equation."))
  :documentation "Notion.so inline equation object found in
`org-notion-rich-text' of type 'equation'.")

;; Stand-alone class for making HTTP requests to the Notion API.
(defclass org-notion-request (org-notion-class)
  ((token
    :initform #'org-notion-token
    :initarg :token
    :documentation "Bearer token used to authenticate requests.")
   (version
    :initform `,org-notion-version
    :initarg :version
    :documentation "Notion API Version.")
   (endpoint
    :initform `,org-notion-endpoint
    :inittarg :endpoint
    :documentation "Notion API endpoint.")
   (method
    :initform current-user
    :initarg :method
    :type symbol
    :documentation "Notion call symbol. See
    `org-notion-method-types' for possible values.")
   (data
    :initform nil
    :initarg :data
    :type (or null string)
    :documentation "Payload to be sent with HTTP request.")
   (callback
    :initform (org-notion-callback-print)
    :initarg :callback
    :documentation "Callback used to handle response from Notion
    API call."))
  :documentation "Notion.so API request.")

(defun org-notion-valid-uuid (str)
  "Validate uuid STR and return it."
  (if (and (stringp str)
	   (string-match-p org-notion-uuid-regexp str))
	   str
	   (signal 'org-notion-invalid-uuid str)))

(cl-defmethod org-notion-dispatch ((obj org-notion-request) &rest args)
  "Dispatch HTTP request with slots from `org-notion-request' OBJ instance."
  (with-slots (token version endpoint method data callback) obj
    (let ((url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (funcall token)))
				       ("Notion-Version" . ,version)
				       ("Content-Type" . "application/json"))))
      (pcase method
	('search (let ((url-request-method "POST")
		       (endpoint (concat endpoint "search"))
		       (url-request-data (if (stringp data) (json-encode `(:query ,data)) data)))
		   (message "%s" (url-retrieve endpoint callback nil nil nil))))
	('current-user
	 (let ((url-request-method "GET")
	       (url (concat endpoint "users/me")))
	   (url-retrieve url callback nil nil nil)))
	('users
	 (let ((url-request-method "GET")
	       (url (concat endpoint "users")))
	   (url-retrieve url callback nil nil nil)))
	('database
	 (let ((url-request-method "GET")
	       (endpoint (concat endpoint (format "databases/%s" (org-notion-valid-uuid data)))))
	   (url-retrieve endpoint callback nil nil nil)))
	('query-database
	 (let ((url-request-method "POST")
	       (url (concat endpoint (format "databases/%s/query" data))))
	   (url-retrieve url callback nil nil nil)))
	('create-database
	 (let ((url-request-method "POST")
	       (url (concat endpoint "databases")))
	   (url-retrieve url callback nil nil nil)))
	('update-database
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "databases/%s")))
	   (url-retrieve url callback nil nil nil)))
	('page
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "pages/%s" (org-notion-valid-uuid data)))))
	   (url-retrieve url callback nil nil nil)))
	('page-property
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "pages/%s/properties/%s" data data))))
	   (url-retrieve url callback nil nil nil)))
	('create-page
	 (let ((url-request-method "POST")
	       (url (concat endpoint "pages")))
	   (url-retrieve url callback nil nil nil)))
	('update-page
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint (format "pages/%s" data))))
	   (url-retrieve url callback nil nil nil)))
	('block
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "blocks/%s" (org-notion-valid-uuid data)))))
	   (url-retrieve url callback nil nil nil)))
	('block-children
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "blocks/%s/children" data))))
	   (url-retrieve url callback nil nil nil)))
	('update-block
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "blocks/%s" data)))
	   (url-retrieve url callback nil nil nil)))
	('append-block
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "blocks/%s/children" data)))
	   (url-retrieve url callback nil nil nil)))
	('delete-block
	 (let ((url-request-method "DELETE")
	       (url (concat endpoint "blocks/%s" data)))
	   (url-retrieve url callback nil nil nil)))
	(_ (signal 'org-notion-invalid-method method))))))

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
(defun org-notion-callback-default ()
  "Read json string from `org-notion-dispatch' status buffer."
  (cl-function
   (lambda (status)
     ;; catch errors if any
     (if (plist-member status :error)
	 (error "HTTP Error %s" (format "%d" (car (last (plist-get status :error)))))
       ;; jump inside the status buffer.
       (with-current-buffer (current-buffer)
	 ;;  quick hack. url.el will always return the body at this position.
	 (search-forward "\n\n")
	 (json-read))))))

(defun org-notion-call (call &optional target callback &rest params)
  "Call the Notion.so API with CALL and optional TARGET, then evaluate CALLBACK with supplied PARAMS."
  (with-slots (token version endpoint method data)
      (org-notion-request)
    (let ((url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (funcall token)))
				       ("Notion-Version" . ,version)
				       ("Content-Type" . "application/json"))))
      (cl-case call
	('current-user (let ((url-request-method "GET")
			     (url (concat endpoint "users/me")))
			 (url-retrieve url (lambda (status)
					     (if (plist-member status :error)
						 (message (format "%d" (car (last (plist-get status :error)))))
					       (message "200"))
					     ))))
	('users (let ((url-request-method "GET")
		      (url (concat endpoint "users")))
		  (url-retrieve url (lambda (_)
				      (with-current-buffer (current-buffer)
					(search-forward "\n\n")
					(print (org-notion--json-read)))))))
	;; TODO 2022-01-12: if ID, just concat, if name or email,
	;; do a lookup in `org-notion-users', else error and prompt
	;; user to use 'users call.
	('user (let ((url-request-method "GET")
		     (url (concat endpoint (format "users/%s" target))))
		 (url-retrieve url (lambda (_)
				     (with-current-buffer (current-buffer)
				       (search-forward "\n\n")
				       (print (org-notion--json-read)))))))
	('search (let ((url-request-method "POST")
		       (url (concat endpoint "search"))
		       (url-request-data (json-encode `(:query ,target))))
		   (url-retrieve url (lambda (_)
				       (with-current-buffer (current-buffer)
					 (search-forward "\n\n")
					 (print (org-notion--json-read)))))))
	('database ())
	('page ())
	('block ())))))

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
		    (with-current-buffer (current-buffer) (switch-to-buffer (current-buffer) (message "success")))))))

;;; Org-mode
(defun org-notion-id-at-point (&optional pom)
  "Get the value of `org-notion-id-property' property key. Checks
headline at POM first, then buffer keywords."
  (org-with-point-at pom
    (or (cdr (assoc org-notion-id-property
		    (org-entry-properties)))
	(cadr (assoc org-notion-id-property
		     (org-collect-keywords `(,org-notion-id-property)))))))

;;;###autoload
(defun org-notion-browse (&optional uuid)
  "Open a Notion.so page by UUID."
  (interactive)
  (if-let ((id (or uuid (org-notion-id-at-point))))
	(browse-url (format "https://www.notion.so/%s" id))
      (message "failed to find %s" org-notion-id-property)))

(provide 'org-notion)
;;; org-notion.el ends here
