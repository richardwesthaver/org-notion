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
;; convenience of Notion with your Org-mode workflow. 

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)

;;; Constants

(defconst org-notion-host "api.notion.com"
  "FQDN of Notion API. This is used to create an entry with
`auth-source-secrets-create'.")

(defconst org-notion-version "2021-08-16"
  "Notion API Version")

(defconst org-notion-max-page-size 100
  "Default count of results returned by Notion. Additional calls
will be made to collect all results using the 'start_cursor'
parameter. Maximum value is 100.")

(defconst org-notion-method-types '(search current-user user
					   users database query-database
					   create-database
					   update-database page
					   page-property create-page
					   update-page block
					   block-children update-block
					   append-block delete-block)
  "Method types available for Notion API requests, used by
`org-notion-request' class.")

(defconst org-notion-block-types '(paragraph heading_1 heading_2
					     heading_3 bulleted_list_item
					     numbered_list_item to_do
					     toggle child_page
					     child_database embed image
					     video file pdf bookmark
					     callout quote code equation
					     divider table_of_contents
					     breadcrumb column column_list
					     link_preview synced_block
					     template link_to_page table
					     table_row unsupported)
  "Type of blocks available for Notion API, used by
`org-notion-block' class. 'unsupported' refers to an unsupported
block type.")

(defconst org-notion-color-types '(default gray brown orange
					   yellow green blue purple pink
					   red gray_background
					   brown_background
					   orange_background
					   yellow_background
					   green_background
					   blue_background
					   purple_background
					   pink_background
					   red_background)
  "Colors available for Notion API text objects, used by
`org-notion-rich-text' class.")

(defconst org-notion-annotation-types '(bold italic strikethrough
					     underline code)
  "Annotations available for Notion API text objects, used by
`org-notion-rich-text' class.")

(defconst org-notion-mention-types '(user page database date link_preview)
  "Mention types available for Notion API text objects, used by
  `org-notion-rich-text' class.")

;;; Custom

(defgroup org-notion nil
  "Customization group for org-notion."
  :tag "Org Notion"
  :group 'org)

(defcustom org-notion-use-auth-source t
  "Check auth-source for Notion integration token. Falls back to
interactive prompt if token isn't found."
  :type 'boolean
  :group 'org-notion)

(defcustom org-notion-coding-system 'utf-8
  "Use custom coding system for org-notion."
  :type 'symbol
  :group 'org-notion)

(defcustom org-notion-completion-ignore-case t
  "org-notion specific value of `completion-ignore-case'"
  :group 'org-notion)

(defcustom org-notion-completion-list t
  "Control the behaviour of org-notion completion functions.
If a list of symbols, specify which fields to complete.

Symbols include
  id (= object uuid)
  type (= sub-type of object)
  parent (= object parent)
  email (= user email address)

If t, completion is done for all of the above.  If nil, no
completion is offered.
  "
  :group 'org-notion
  :type '(choice (const :tag "No Completion" nil)
		 (const :tag "Complete all fields" t)
		 (repeat :tag "Field"
			 (choice (const id)
				 (const type)
				 (const parent)
				 (const email)))))

(defcustom org-notion-push-hook nil
  "Hook to run after `org-notion-push'"
  :type 'hook
  :group 'org-notion)

(defcustom org-notion-pull-hook nil
  "Hook to run after `org-notion-pull'"
  :type 'hook
  :group 'org-notion)

;;; Vars

(defvar org-notion-endpoint (format "https://%s/v1/" org-notion-host)
  "URI of Notion API endpoint")

(defvar org-notion-id-property "NOTION_ID"
  "Name of NOTION_ID Org-mode property.")

(defvar org-notion-uuid-regexp "\\<[[:xdigit:]]\\{8\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{12\\}\\>"
  "A regular expression matching a UUID with or without hyphens.")

(defvar org-notion-hashtable (make-hash-table :test #'equal)
  "Hashtable for `org-notion-class' instances.")

(defvar org-notion-page-cache (make-hash-table :test #'equal)
  "Cache of Notion pages.")

(defvar org-notion-database-cache (make-hash-table :test #'equal)
  "Cache of Notion databases.")

(defvar org-notion-block-cache (make-hash-table :test #'equal)
  "Cache of Notion blocks.")

(defvar org-notion-object-tracker nil
  "List of active `org-notion-object' instances.")

(defvar org-notion-proc-buffer "org-notion-proc"
  "Name of the org-notion process buffer.")

(defvar org-notion-verbosity 'debug)

;;; Errors

(define-error 'org-notion-error "Unknown org-notion error")

(define-error 'org-notion-invalid-uuid "Invalid UUID" 'org-notion-error)
(define-error 'org-notion-invalid-method "Invalid API method" 'org-notion-error)

;; API Responses
(define-error 'org-notion-bad-request "HTTP 400 Bad Request" 'org-notion-error)
(define-error 'org-notion-unauthorized "HTTP 401 Unauthorized" 'org-notion-error)
(define-error 'org-notion-restricted-resource "HTTP 403 Restricted Resource" 'org-notion-error)
(define-error 'org-notion-not-found "HTTP 404 Object not found" 'org-notion-error)
(define-error 'org-notion-conflict "HTTP 409 Conflict error possibly due to data collision" 'org-notion-error)
(define-error 'org-notion-rate-limited "HTTP 429 Exceeded number of requests allowed" 'org-notion-error)
(define-error 'org-notion-unexpected "HTTP 500 Internal server error occurred" 'org-notion-error)
(define-error 'org-notion-unavailable "HTTP 503 Notion is unavailable" 'org-notion-error)
(define-error 'org-notion-database-unavailable "HTTP 503 Notion database service is unavailable" 'org-notion-error)

;;; Utils

(defun org-notion-log (&rest s)
  (when (eq 'debug org-notion-verbosity)
    (message "%s" s)))

(defmacro oref-or (obj slot)
  "Get the value of object or class."
  (declare (debug (form symbolp form)))
  `(cond
    ((class-p ,obj) (eieio-oref-default ,obj ',slot))
    ((eieio-object-p ,obj) (eieio-oref ,obj ',slot))))

(defmacro oset-or (obj slot value)
  "Set the value of object or class."
  (declare (debug (form symbolp form)))
  `(cond
    ((class-p ,obj) (eieio-oset-default ,obj ',slot ,value))
    ((eieio-object-p ,obj) (eieio-oset ,obj ',slot ,value))))

(defsubst org-notion-string= (str1 str2)
  "Return t if strings STR1 and STR2 are equal, ignoring case."
  (and (stringp str1) (stringp str2)
       (eq t (compare-strings str1 0 nil str2 0 nil t))))

;;;; ID Utils

(defun org-notion-uuid-p (str)
  "Return t if STR is a uuid, else nil."
  (and (stringp str)
       (string-match-p org-notion-uuid-regexp str)))

(defun org-notion-valid-uuid (str)
  "Validate uuid STR and return it."
  (if (org-notion-uuid-p str)
      str
    nil))

(defsubst org-notion-id= (id1 id2)
  "Return t if ids ID1 and ID2 are equal."
  (and (org-notion-uuid-p id1) (org-notion-uuid-p id2)
       (eq t (compare-strings id1 0 nil id2 0 nil))))

;;;; Org Utils

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "notion"
			     :follow 'org-notion-browse
			     :complete (lambda ()
					 (format "notion:%s"))))
    
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

(defun org-notion-id-at-point (&optional pom)
  "Get the value of `org-notion-id-property' property key. Checks
headline at POM first, then buffer keywords."
  (org-with-point-at pom
    (or (cdr (assoc org-notion-id-property
		    (org-entry-properties)))
	(cadr (assoc org-notion-id-property
		     (org-collect-keywords `(,org-notion-id-property)))))))

;;;; Callbacks

(defmacro org-notion-with-callback (&rest body)
  "Evaluate BODY as a callback for `org-notion-dispatch'"
  (declare (debug t)
	   (indent 0))
  `(lambda (response)
     (with-current-buffer (current-buffer)
       ;;  quick hack. url.el will always return body at this position.
       (search-forward "\n\n")
       (let ((json-data (json-read)))
	 (if (equal (cdar json-data) "error")
	     (org-notion-log (format "HTTP %d: %s"
				     (alist-get 'status json-data)
				     (alist-get 'message json-data)))
	   ,@body)))))

(defun org-notion-callback-default ()
  "Read json string from `org-notion-dispatch' status buffer and print output"
  (org-notion-with-callback (print json-data)))


;;;; Auth

(defun org-notion-token (&optional token)
  "Find the Notion API Integration Token.
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

;;; OOP

;; Default superclass. This is inherited by all other org-notion
;; classes and should only define new static methods. The only method
;; implemented is `org-notion-print' which simply prints the fields of
;; a class instance.
(defclass org-notion-class nil
  nil
  :documentation "Default superclass inherited by `org-notion' classes."
  :abstract "Class org-notion-class is abstract.
use `org-notion-object' `org-notion-rich-text' or `org-notion-request' to create instances.")

(cl-defmethod org-notion-print ((obj org-notion-class))
  "Pretty-print EIEIO class objects as string."
  (let ((slots (mapcar (lambda (slot) (aref slot 1)) (eieio-class-slots (eieio-object-class obj)))))
    (setq slots (cl-remove-if (lambda (s) (not (slot-boundp obj s))) slots))
    (apply #'concat
	   (mapcar (lambda (slot)
		     (let ((slot (intern (pp-to-string slot))))
		       (format "%+4s:   %s\n" slot (slot-value obj (intern (pp-to-string slot))))))
		   slots))))

;;;; Requests

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
    :type (or null list)
    :documentation "Payload to be sent with HTTP request.")
   (callback
    :initform (org-notion-callback-default)
    :initarg :callback
    :documentation "Callback used to handle response from Notion
    API call."))
  :documentation "Notion API request.")

(cl-defmethod org-notion-dispatch ((obj org-notion-request))
  "Dispatch HTTP request with slots from `org-notion-request' OBJ instance."
  (with-slots (token version endpoint method data callback) obj
    (let ((url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (funcall token)))
				       ("Notion-Version" . ,version)
				       ("Content-Type" . "application/json")))
	  (callback (or callback (org-notion-callback-default))))
      (pcase method
	('search (let ((url-request-method "POST")
		       (endpoint (concat endpoint "search"))
		       (url-request-data (json-encode data)))
		   (url-retrieve endpoint callback nil nil nil)))
	('current-user
	 (let ((url-request-method "GET")
	       (url (concat endpoint "users/me")))
	   (url-retrieve url callback nil nil nil)))
	('user
	 (let ((url-request-method "GET")
	       ;; FIX 2022-01-20: this doesn't work
	       (url (concat endpoint "users/" (org-notion-valid-uuid data))))
	   (url-retrieve url callback nil nil nil)))
	('users
	 (let ((url-request-method "GET")
	       (url (concat endpoint "users/")))
	   (url-retrieve url callback nil nil nil)))
	('database
	 (let ((url-request-method "GET")
	       (endpoint (concat endpoint "databases/" (org-notion-valid-uuid data))))
	   (url-retrieve endpoint callback nil nil nil)))
	;; filter: {}
	;; sorts: <>
	;; start_cursor: string
	;; page_size: int
	('query-database
	 (let ((url-request-method "POST")
	       (url (concat endpoint (format "databases/%s/query" data))))
	   (url-retrieve url callback nil nil nil)))
	;; parent: {type:page_id page_id:id}
	;; title: rich-text
	;; properties: {}
	('create-database
	 (let ((url-request-method "POST")
	       (url (concat endpoint "databases")))
	   (url-retrieve url callback nil nil nil)))
	;; parent: {type:page_id page_id:id}
	;; title: rich-text
	;; properties: {}
	('update-database
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "databases/%s" (org-notion-valid-uuid data))))
	   (url-retrieve url callback nil nil nil)))
	('page
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "pages/%s" (org-notion-valid-uuid data)))))
	   (url-retrieve url callback nil nil nil)))
	('page-property
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "pages/%s/properties/%s" data data))))
	   (url-retrieve url callback nil nil nil)))
	;; parent: {type:(page_id/database_id) X_id:id}
	;; properties: {}
	;; children: <>
	;; icon: {}
	;; cover: {}
	('create-page
	 (let ((url-request-method "POST")
	       (url (concat endpoint "pages")))
	   (url-retrieve url callback nil nil nil)))
	;; properties: {}
	;; archived: bool
	;; icon: {}
	;; cover: {}
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
	;; type: (text/checked)
	;; archived: bool
	('update-block
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "blocks/%s" data)))
	   (url-retrieve url callback nil nil nil)))
	;; children: <blocks>
	('append-block
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "blocks/%s/children" data)))
	   (url-retrieve url callback nil nil nil)))
	('delete-block
	 (let ((url-request-method "DELETE")
	       (url (concat endpoint "blocks/%s" data)))
	   (url-retrieve url callback nil nil nil)))
	(_ (signal 'org-notion-invalid-method method))))))

(defun org-notion-search-data (query &optional sort filter start page_size)
  "Prepare data for Notion search. Returns a list where car is
QUERY string and cdr is a json object containing optional values.

SORT is either \"ascending\" or \"descending\".
FILTER is either \"page\" or \"database\".
START AND PAGE_SIZE are integers."
  (let (data)
    (push `(query . ,query) data)
    (when sort
      (if (and (stringp sort)
	       (or (string= sort "ascending")
		   (string= sort "descending")))
	  (nconc data `((sort . ((direction . ,sort) (timestamp . "last_edited_time")))))
	(org-notion-log "SORT must be either 'ascending' or 'descending'")))
    (when filter
      (if (and (stringp filter)
	       (or (string= filter "page")
		   (string= filter "database"))) 
	  (nconc data `((filter . ((value . ,filter) (property . "object")))))
	(org-notion-log "FILTER must be either 'page' or 'database'")))
    (when page_size
      (if (and (integerp page_size)
	       (>= org-notion-max-page-size page_size))
	  (nconc data `((page_size . ,page_size)))
	(org-notion-log
	 (format "PAGE_SIZE must be an integer less than or equal to %s"
		 org-notion-max-page-size))))
    (when start
      (if (org-notion-uuid-p start)
	  (nconc data `((start_cursor . ,start)))
	(org-notion-log "START must be a valid UUID")))
    data))

(defun org-notion-database-data ())

(defun org-notion-database-query-data ())

(defun org-notion-page-data ())

(defun org-notion-block-data ())

;;;; Cache

(defsubst org-notion-objects (&optional class)
  "Return a list of all org-notion class instances. if CLASS is
given, only show instances of this class."
  (if class
          (seq-filter
       (lambda (o)
	   (same-class-p o class))
       (hash-table-values org-notion-hashtable))
    (hash-table-values org-notion-hashtable)))

(defun org-notion-clear-cache ()
  "Set all internal org-notion vars to nil."
  (clrhash org-notion-hashtable))

(defun org-notion-hash-p (key obj pred)
  "Throw `org-notion-hash-ok' non-nil if KEY matches OBJ according to PRED.
PRED may take the same values as the elements of `org-notion-completion-list'."
  (if (and (memq 'id pred)
	   (org-notion-string= key (org-notion-id obj)))
      (throw 'org-notion-hash-ok 'id))
  (if (and (memq 'type pred)
	   (org-notion-string= key (oref obj :type)))
      (throw 'org-notion-hash-ok 'type))
  (if (and (memq 'parent pred)
	   (org-notion-string= key (oref obj :parent)))
      (throw 'org-notion-hash-ok 'parent))
  (if (and (memq 'email pred)
	   (org-notion-string= key (oref obj :email)))
      (throw 'org-notion-hash-ok 'email)))

(defun org-notion-gethash (key &optional pred)
  "Return objects associated with KEY in `org-notion-hashtable'.
KEY must be a string or nil. Empty strings and nil are
ignored. PREDICATE may take the same values as
`org-notion-completion-list'. If the symbol id is used, return
a single object, otherwise return a list.'"
  (when (not (string-empty-p key))
    (let ((all-objs (gethash key org-notion-hashtable))
	   objs)
      (if (or (not pred) (eq t pred))
	  all-objs)
      (if (eql pred 'id)
	  (car all-objs)
	(dolist (o all-objs objs)
	  (if (catch 'org-notion-hash-ok
		(org-notion-hash-p key o pred))
	      (push o objs)))))))

(defun org-notion-puthash (obj table)
  "Add OBJ to `org-notion-hashtable'. The key is the id slot, and
value is OBJ. Empty or nil id slots are ignored."
  (let ((key (oref obj :id)))
    (if (and key (not (string-empty-p key)))
	(let ((objs (gethash key table)))
	  (puthash key (if objs
			   (cl-pushnew obj objs)
			 (list obj))
		   table)))))

(defun org-notion-remhash (key obj)
  "Remove OBJ from list of objects associated with KEY. KEY must be
a string or nil. Empty strings and nil are ignored."
  (if (and key (not (string-empty-p key)))
      (let* ((key (downcase key))
	     (objs (gethash key org-notion-hashtable)))
	(when objs
	  (setq objs (delq obj objs))
	  (if objs
	      (puthash key objs org-notion-hashtable)
	    (remhash key org-notion-hashtable))))))

(defun org-notion-hash-update (obj old new)
  "Update hash for OBJ. Remove OLD, insert NEW. both OLD and NEW
are lists of values."
  (dolist (i old)
    (org-notion-remhash i obj))
  (dolist (i new)
    (org-notion-puthash i obj)))

;; Cache class inherited by org-notion-object
(defclass org-notion-cache (org-notion-class)
  ((cache :type symbol
	  :allocation :class
	  :documentation "The symbol used to maintain a hashtable
		 of org-notion instances. The instance hashtable
		 is treated as a variable, with new instanaces
		 added to it.")
   :documentation "Mixin used to cache object instances based on
   `eieio-instance-tracker'. Inheritors must override
   `cache' which is a variable used to cache instances."
   :abstract t))

(cl-defmethod initialize-instance :after ((this org-notion-cache) &rest _slots)
  "Make sure THIS is in our cache. Optional argument SLOTS are the
initialization arguments. This will not update a duplicate
hash. The old one is always kept."
  (let ((table (symbol-value (oref this cache)))
	(key (oref this :id)))
    (if (not (gethash key table))
	(puthash key this table)
      (org-notion-log (format "duplicate key detected: %s" key)))))

(cl-defmethod delete-instance ((this org-notion-cache))
  "Remove THIS from cache."
  (remhash (oref this :id) (symbol-value (oref this cache))))

(cl-defmethod update-instance ((this org-notion-cache))
  "Update hash of THIS in cache."
  (puthash (oref this :id) this (symbol-value (oref this cache))))

;;;; Generics

;; The following generics are implemented by `org-notion-object'
;; subclasses.
(cl-defgeneric org-notion-from-json (obj json)
  "Interpret JSON as `org-notion-object'")

(cl-defgeneric org-notion-to-json (obj)
  "Interpret `org-notion-object' OBJ as json object.")

(cl-defgeneric org-notion-from-org (obj str)
  "Interrpret Org-mode STR as `org-notion-object'")

(cl-defgeneric org-notion-to-org (obj &optional type)
  "Interpret `org-notion-object' OBJ as Org-mode syntax TYPE.")

;;;; Objects

;; Parent class of Notion objects. The 'id' slot is inherited by
;; subclasses. 'cache' is a variable used to store instances.
(defclass org-notion-object (org-notion-class org-notion-cache)
    ((id
    :initform nil
    :initarg :id
    :type (or null string)
    :accessor org-notion-id
    :documentation "UUID v4 associated with this object")
   (cache :initform 'org-notion-hashtable))
    :documentation "Top-level class for Notion API objects.")

;;;;; User
(defclass org-notion-user (org-notion-object)
  ((type
    :initform ""
    :initarg :type
    :type string
    :accessor org-notion-user-type
    :documentation "Type of the user. This slot should be either
    'person' or 'bot'.")
   (name
    :initform ""
    :initarg :name
    :type string
    :accessor org-notion-user-name
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
    :accessor org-notion-user-email
    :documentation "Email address of a user. Only present if
    `:type' is 'person' and integration has user capabilities
    that allow access to email addresses.")
   (owner
    :initform nil
    :initarg :owner
    :type (or null string)
    :documentation "The owner of a bot user -- either 'workspace'
    or id of user."))
  :documentation "Notion user object - can be a real person or
a bot. Identified by the `:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-user) json)
  (if (string= "user" (alist-get 'object json))
      (progn
	(oset obj :id (alist-get 'id json))
	(oset obj :type (alist-get 'type json))
	(oset obj :name (alist-get 'name json))
	(oset obj :avatar (alist-get 'avatar_url json))
	(oset obj :email (alist-get 'email json))
	(oset obj :owner (cdadar (alist-get 'bot json))))
    (error "expected user object, found %s" (alist-get 'object json)))
  obj)

(cl-defmethod org-notion-to-json ((obj org-notion-user)))

;;;;; Database

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
    :accessor org-notion-properties
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
    :accessor org-notion-url
    :documentation "The URL of the Notion database."))
  :documentation "Notion database object - identified by the
`:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-database) json)
  ;; quick check
  (if (string= (alist-get 'object json) "database")
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

;;;;; Page

(defclass org-notion-page (org-notion-object)
  ((created
    :initform (format-time-string "%Y-%m-%d %T")
    :initarg :created
    :documentation "Datetime when this page was created.")
   (updated
    :initform (format-time-string "%Y-%m-%d %T")
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
    :accessor org-notion-properties
    :documentation "Property values of this page.")
   (parent
    :initform nil
    :initarg :parent
    :type (or null string)
    :documentation "The parent id of this page. Can be a database,
    page, or workspace.")
   (url
    :initform ""
    :initarg :url
    :type string
    :accessor org-notion-url
    :documentation "The URL of the Notion page."))
  :documentation "Notion page object - identified by the `:id'
slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-page) json)
  (if (string= "page" (alist-get 'object json))
      (progn
	(oset obj :id (alist-get 'id json))
	(oset obj :created (alist-get 'created_time json))
	(oset obj :updated (alist-get 'last_edited_time json))
	(oset obj :archived (unless (alist-get 'archived json) t))
	(oset obj :icon (alist-get 'icon json))
	(oset obj :cover (alist-get 'cover json))
	(oset obj :parent (cdadr (alist-get 'parent json)))
	(oset obj :url (alist-get 'url json)))
    (error "expected page object, found %s" (alist-get 'object json)))
  obj)

(cl-defmethod org-notion-to-json ((obj org-notion-page)))

(cl-defmethod org-notion-from-org ((obj org-notion-page) str))

(cl-defmethod org-notion-to-org ((obj org-notion-page) &optional type)
  (with-slots (id created updated properties parent) obj
    (pcase type
      ((or 'nil 'headline) (org-element-interpret-data
			    `(headline
			      (:title "mock" :level 1 ; FIXME
				      :CREATED (org-notion-to-org-time created)
				      :UPDATED (org-notion-to-org-time updated))
			      (property-drawer nil ((node-property (:key "NOTION_ID" :value ,id))
						    (node-property (:key "CREATED" :value ,created))
						    (node-property (:key "UPDATED" :value ,updated)))))))
      ('file (org-element-interpret-data
	      `(org-data nil (section nil)
			 (keyword (:key "TITLE" :value "org-notion"))
			 (keyword (:key "NOTION_ID" :value ,id))
			 (keyword (:key "CREATED" :value ,created))
			 (keyword (:key "UPDATED" :value ,updated)))))
      (_ (error "invalid org-element type %s" type)))))

;;;;; Block

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
    nested within it.")
   (properties
    :initform nil
    :initarg :properties
    :accessor org-notion-properties
    :documentation "alist where KEY is property type and VAL is
     value.")
   (text
    :initform nil
    :initarg :text
    :type (or null vector)
    :documentation "Vector of rich-text objects associated with
    this block, if any.")
   (children
    :initform nil
    :initarg :children
    :type (or null vector)
    :documentation "Vector of block objects which are children of
    this block, if any."))
  :documentation "Notion block object - identified by the
`:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-block) json)
  (if (string= "block" (alist-get 'object json))
      (progn
	(oset obj :id (alist-get 'id json))
	(oset obj :type (intern (alist-get 'type json)))
	(oset obj :created (alist-get 'created_time json))
	(oset obj :updated (alist-get 'last_edited_time json))
	(oset obj :archived (unless (alist-get 'archived json) t))
	(oset obj :has_children (unless (alist-get 'has_children json) t))
	(pcase (oref obj :type)
	  ('paragraph ()) 		; text
	  ('heading_1 ()) 		; text
	  ('heading_2 ()) 		; text
	  ('heading_3 ()) 		; text
	  ('bulleted_list_item ()) 	; text + children
	  ('numbered_list_item ()) 	; text + children
	  ('to_do ()) 			; text + children + properties
	  ('toggle ()) 			; text + children
	  ('child_page 			; properties
	   (oset obj :properties (car (alist-get 'child_page json)))) 
	  ('child_database 		; properties
	   (oset obj :properties (car (alist-get 'child_database json))))
	  ('embed 			; properties
	   (oset obj :properties (car (alist-get 'embed json))))
	  ('image ()) ; file https://developers.notion.com/reference/file-object
	  ('video ()) ; file
	  ('file ())  ; file + text
	  ('pdf ())   ; file
	  ('bookmark ()) 		; text + properties
	  ('callout ()) 		; text + properties + children + file/emoji
	  ('quote ()) 			; text + children
	  ('code ()) 			; text + properties
	  ('equation ()) 		; properties
	  ('divider ()) 		; nil
	  ('table_of_contents ()) 	; nil
	  ('breadcrumb ()) 		; nil
	  ('column ()) 			; children
	  ('column_list ()) 		; children
	  ('link_preview ()) 		; properties
	  ('synced_block ()) 		; properties + children
	  ('template ()) 		; text + children
	  ('link_to_page ()) 		; properties
	  ('table ()) 			; properties
	  ('table_row ()) 		; children
	  ('unsupported ())
	  (_ ())))
    (error "expected block object, found %s" (alist-get 'object json)))
  obj)

(cl-defmethod org-notion-to-json ((obj org-notion-block)))

(cl-defmethod org-notion-to-org ((obj org-notion-block) &optional _))

;;;;; Text

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
  :documentation "Notion rich text object.")

(cl-defmethod org-notion-from-json ((obj (subclass org-notion-rich-text))))
(cl-defmethod org-notion-to-json ((obj (subclass org-notion-rich-text))))
(cl-defmethod org-notion-from-org ((obj (subclass org-notion-rich-text)) str))
(cl-defmethod org-notion-to-org ((obj (subclass org-notion-rich-text)) &optional type))

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
  :documentation "Notion inline text object found in
`org-notion-rich-text' of type 'text'")

(defclass org-notion-inline-mention (org-notion-rich-text)
  ((mention_type
    :initform nil
    :initarg :mention_type
    :type symbol
    :documentation "Type of the inline mention. See
    `org-notion-mention-types' for a list of possible values."))
  :documentation "Notion inline mention object found in
`org-notion-rich-text' of type 'mention'")

(defclass org-notion-inline-equation (org-notion-rich-text)
  ((expression
    :initform ""
    :initarg :expression
    :type string
    :documentation "The LaTeX string representing this inline
    equation."))
  :documentation "Notion inline equation object found in
`org-notion-rich-text' of type 'equation'.")

;;; API Commands

;;;###autoload
(defun org-notion-get-current-user ()
  "Retrieve the bot user associated with the current
`org-notion-token'"
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'current-user
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "user")
		  (org-notion-log (format "current user: %s\nid: %s"
					  (alist-get 'name json-data)
					  (alist-get 'id json-data))))))))

;;;###autoload
(defun org-notion-get-users ()
  "Get all users in the Notion workspace. This will return a 403
status code if your integration doesn't have User Capabilities
enabled."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'users
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "list")
		  (let ((results (alist-get 'results json-data)))
		    (dolist (i (append results nil))
		      (org-notion-log (format "name: %s | id: %s"
					      (alist-get 'name i)
					      (alist-get 'id i))))))))))

;;;###autoload
(defun org-notion-search (query &optional sort filter)
  "Search the Notion workspace using QUERY and optional SORT and FILTER values.
SORT should be \"ascending\" or \"descending\" and FILTER should
be \"page\" or \"database\"."
  (interactive
   "squery: ")
  (org-notion-dispatch
   (org-notion-request
    :method 'search
    :data (org-notion-search-data query sort filter)
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "list")
		  (let ((results (alist-get 'results json-data)))
		    (org-notion-log results)))))))

;;; Org-mode Commands

;;;###autoload
(defun org-notion-browse (&optional uuid)
  "Open a Notion page by UUID."
  (interactive)
  (if-let ((id (or uuid (org-notion-id-at-point))))
      (browse-url (format "https://www.notion.so/%s" id))
    (message "failed to find %s" org-notion-id-property)))

;;;###autoload
(defun org-notion-push (&optional buf create)
  "Push the current headline to Notion. If BUF is non-nil push the
entire buffer. If CREATE is non-nil also create new objects."
  (interactive))

;;;###autoload
(defun org-notion-pull (&optional buf)
  "Pull and update the current headline from Notion. If BUF is
non-nil pull updates for entire buffer."
  (interactive))

(provide 'org-notion)
;;; org-notion.el ends here
