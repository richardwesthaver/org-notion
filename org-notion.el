;;; org-notion.el --- Notion.so + Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ellis

;; Author: ellis <ellis@rwest.io>
;; Version: 0.1.0
;; Keywords: wp, extensions
;; Package-Requires: ((emacs "28.1"))

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

(eval-when-compile (require 'cl-lib))
(require 'org)
(require 'org-element)
(require 'eieio)
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
will be made to collect all results using the `start_cursor'
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
`org-notion-block' class. `unsupported' refers to an unsupported
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

(defconst org-notion-property-types '(rich_text number select multi_select
						date formula relation rollup
						title people files checkbox
						url email phone_number created_time
						created_by last_edited_time last_edited_by)
  "Notion default property_item types.")

(defconst org-notion-parent-types '(database page workspace block)
  "Notion parent object types.")

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
  "Custom coding system for org-notion."
  :type 'symbol
  :group 'org-notion)

(defcustom org-notion-cache-enable t
  "Enable org-notion object cache if value is non-nil. Note that the
default caching behavior is to write a message and return nil when
attempting to insert a duplicate object, which occurs often in
testing and cause `should' to return unexpected results."
  :type 'boolean
  :group 'org-notion)

(defcustom org-notion-cache-overwrite nil
  "Enable org-notion-cache overwrite on insert if value is non-nil. By
default, duplicate inserts into the cache will log a message
without modifying the current value. When this is enabled, a
message is still logged (disable with `org-notion-verbosity'),
but the current value is replaced."
  :group 'org-notion
  :type 'boolean)

(defcustom org-notion-completion-ignore-case t
  "Org-notion specific value of `completion-ignore-case'"
  :group 'org-notion
  :type 'boolean)

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

(defcustom org-notion-keymap-prefix "C-c n"
  "Prefix for org-notion-mode keybinds."
  :type 'string
  :group 'org-notion)

;;; Vars

(defvar org-notion-endpoint (format "https://%s/v1/" org-notion-host)
    "URI of Notion API endpoint")

(defvar org-notion-uuid-regexp "\\<[[:xdigit:]]\\{8\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{4\\}-?[[:xdigit:]]\\{12\\}\\>"
  "A regular expression matching a UUID with or without hyphens.")

(defvar org-notion-hashtable (make-hash-table :test #'equal)
  "Hashtable for `org-notion-class' instances.")

(defvar org-notion-verbosity 'debug
  "Level of verbosity for `org-notion' logging.")

(defvar org-notion-current-user nil
  "Current Notion user. This may be a user created via Notion API
integration.")

(defvar org-notion-dispatch-sync-sleep 0.05
  "Time to sleep for between checks on
`org-notion-last-dispatch-result'. Only applies to synchronous
calls to `org-notion-dispatch' (:async nil).")

(defvar org-notion-last-dispatch-result nil
  "value of last `org-notion-dispatch' call.")

(defvar org-notion-field-org-keys
  '((id . "NOTION_ID")
    (user . "NOTION_USER")
    (email . "NOTION_EMAIL")
    (cover . "NOTION_COVER")
    (icon . "NOTION_ICON")
    (created . "CREATED")
    (updated . "UPDATED")
    (parent . "NOTION_PARENT")
    (archived . "NOTION_ARCHIVED")
    (url . "NOTION_URL")
    (link . "NOTION_LINK")
    (content . "NOTION_CONTENT")
    (mention-type . "NOTION_MENTION")
    (expression . "NOTION_EXPRESSION")
    (href . "NOTION_HREF")
    (annotations . "NOTION_ANNOTATIONS")
    (color . "NOTION_COLOR")
    (properties . "NOTION_PROPERTIES")
    (children . "NOTION_CHILDREN")
    (text . "NOTION_TEXT")
    (type . "NOTION_TYPE"))
  "Mapping of org-notion EIEIO field names to org-element
node-property or keyword names (:key).")

(defvar org-notion-field-abbrevs (mapcar #'car org-notion-field-org-keys)
  "Org-notion field short names.")

(defvar org-notion-field-names (mapcar #'cdr org-notion-field-org-keys)
  "Org-notion field full names as string.")

(defvar org-notion-class-keys
  '((database . org-notion-database)
    (user . org-notion-user)
    (page . org-notion-page)
    (rich-text . org-notion-rich-text)
    (inline-text . org-notion-inline-text)
    (inline-mention . org-notion-inline-mention)
    (inline-equation . org-notion-inline-equation)
    (block . org-notion-block))
  "Mapping of short names such as \"database\" to
org-notion-class type names, i.e. org-notion-database.")

(defvar org-notion-class-abbrevs
  (flatten-list (mapcar #'(lambda (i) (car i)) org-notion-class-keys))
  "`org-notion-class' type abbreviations.")

(defvar org-notion-classes
  (flatten-list (mapcar #'(lambda (i) (cdr i)) org-notion-class-keys))
  "List of `org-notion-class' symbols.")

;;; Errors

(define-error 'org-notion-error "Unknown org-notion error")
(define-error 'org-notion-invalid-key "Invalid key value" 'org-notion-error)
(define-error 'org-notion-invalid-uuid "Invalid UUID" 'org-notion-error)
(define-error 'org-notion-invalid-object "Invalid object" 'org-notion-error)
(define-error 'org-notion-invalid-method "Invalid API method" 'org-notion-error)
(define-error 'org-notion-invalid-element-type "Invalid Org element type" 'org-notion-error)
(define-error 'org-notion-bad-time "Bad time spec" 'org-notion-error)
(define-error 'org-notion-invalid-page-size
	      (format "PAGE_SIZE must be an integer less than or equal to %s"
		      org-notion-max-page-size)
	      'org-notion-error)

;; API Responses
(define-error 'org-notion-bad-request "HTTP 400 Bad Request" 'org-notion-error)
(define-error 'org-notion-unauthorized "HTTP 401 Unauthorized" 'org-notion-error)
(define-error 'org-notion-restricted-resource "HTTP 403 Restricted Resource" 'org-notion-error)
(define-error 'org-notion-not-found "HTTP 404 Object not found" 'org-notion-error)
(define-error 'org-notion-conflict "HTTP 409 Conflict error possibly due to data collision" 'org-notion-error)
(define-error 'org-notion-rate-limited "HTTP 429 Exceeded number of requests allowed" 'org-notion-error)
(define-error 'org-notion-unexpected "HTTP 500 Internal server error occurred" 'org-notion-error)
(define-error 'org-notion-unavailable "HTTP 503 Resource is unavailable" 'org-notion-error)

(defun org-notion--http-error (status)
  "Return an `org-notion-error' type based on STATUS."
  (pcase status
    (400 'org-notion-bad-request)
    (401 'org-notion-unauthorized)
    (403 'org-notion-restricted-resource)
    (404 'org-notion-not-found)
    (409 'org-notion-conflict)
    (429 'org-notion-rate-limited)
    (500 'org-notion-unexpected)
    (503 'org-notion-unavailable)
    (_ 'org-notion-error)))

(defun org-notion--handle-http-error (json)
  "Check Notion response in JSON for an error and handle it by
signaling `org-notion-error' types."
  (when (equal (cdar json) "error")
    (let ((err (org-notion--http-error (alist-get 'status json)))
	  (msg (alist-get 'message json)))
      (signal err msg))))

;;; Utils

;;;###autoload
(def-edebug-elem-spec 'org-notion-place '(form))

(defmacro org-notion-log (str &rest args)
  "Log a message with format STR given ARGS"
  `(message ,str ,@args))

(defun org-notion-dbg (&rest s)
  "Print an object."
  (when (or (eq org-notion-verbosity t)
	    (eq org-notion-verbosity 'debug))
    (princ s)))

(defun oref-or (obj slot)
  "Get the value of object or class."
  (cond
    ((class-p obj) (eieio-oref-default obj slot))
    ((eieio-object-p obj) (eieio-oref obj slot))))

(defun oref-and (obj &rest slots)
  "Return a list of values bound to SLOTS in OBJ. Uses `oref-or' internally."
  (mapcar (lambda (x) (oref-or obj x)) slots))

(defun oset-or (obj slot value)
  "Set the value of object or class."
  (cond
    ((class-p obj) (eieio-oset-default obj slot value))
    ((eieio-object-p obj) (eieio-oset obj slot value))))

(defun oset-and (obj &rest args)
  "Set each SLOT to VALUE in OBJ. Uses `oset-or' internally."
  (if (/= (logand (length args) 1) 0)
      (signal 'wrong-number-of-arguments (list 'oset-and (length args)))
      (while args (oset-or obj (pop args) (pop args)))))

(defun org-notion-field-assoc (sym)
  "Return the cdr of `org-notion-field-names' associated with car SYM
in `org-notion-field-org-keys'."
  (when-let ((res (assoc sym org-notion-field-org-keys)))
    (cdr res)))

(defun org-notion-field-rassoc (str)
  "Return the member of `org-notion-field-abbrevs' associated with cdr
STR in `org-notion-field-org-keys'."
  (when-let ((res (rassoc str org-notion-field-org-keys)))
    (car res)))

(defun org-notion-class-assoc (sym)
  "Return the member of `org-notion-classes' associated with car SYM
in `org-notion-class-keys'."
  (when-let ((res (rassoc str org-notion-class-keys)))
    (cdr res)))

(defun org-notion-class-rassoc (str)
  "Return the member of `org-notion-class-abbrevs' associated with cdr
STR in `org-notion-class-keys'."
  (when-let ((res (rassoc str org-notion-class-keys)))
    (car res)))

(defun org-notion--kv (key val)
  (list :key key :value val))

(defun org-notion--node-prop (key val)
  "Create an org-element node-property given KEY and VAL"
  `(node-property ,(org-notion--kv key val)))

(defun org-notion--kw (key val)
  "Create an org-element keyword given KEY and VAL."
  `(keyword ,(org-notion--kv key val)))

(defun org-notion--prop (key val &optional type)
  "Create an org-element TYPE given KEY and VAL.
If KEY is a non-nil symbol it is looked up in
`org-notion-field-org-keys' via `org-notion-field-assoc' and
replaced with the associated string value."
  (let ((key (pcase key
	       ((and (pred symbolp) (pred (not null))) (org-notion-field-assoc key))
	       ((pred stringp) key)
	       (_ (signal 'org-notion-invalid-key key)))))
    (unless (null val) (pcase type
       ((or 'kw 'keyword) (org-notion--kw key val))
       ((or 'nil 'prop 'node-prop 'node-property)
	(org-notion--node-prop key val))))))

(defun org-notion--property-drawer (alist)
  "Given an ALIST of (KEY . VAL) pairs. create an org-element
property-drawer."
  (let ((res 'property-drawer)
	(props))
    (dolist (kv alist props)
      (when-let ((k (car kv)) (v (cdr kv)))
	(push (org-notion--prop k v 'prop) props)))
    (setq res (list res nil props))
    res))

(defun org-notion-parse-parent (json)
  "Get the parent type and id. Return a cons cell (TYPE . ID)."
  (let* ((parent (alist-get 'parent json))
	 (type (alist-get 'type parent))
	 (id (alist-get (intern type) parent)))
    (cons type id)))

;; TODO 2023-03-06: for `org-notion-from-org'
(defun org-notion-parse-user-str (str)
  "Parse a string containing the User's name, email, or both."
  nil)

(defun org-notion--get-results (json)
  (when (equal (alist-get 'object json) "list")
    (alist-get 'results json)))

(defun org-notion-filter-results (json-array obj-typ)
  "Filter JSON-ARRAY (an array of results from the Notion API),
 where :object = OBJ-TYP (a string)."
  (seq-filter (lambda (a) (if (equal obj-typ (cdar a)) t nil)) json-array))

(defun org-notion--get-flag (sym json)
  "Return the value of SYM in alist JSON. If value is :json-false, return nil."
  (let ((val (alist-get sym json)))
    (cond
     ((eq val :json-false) nil )
     ('_ val))))

(defsubst org-notion-string= (str1 str2)
  "Return t if strings STR1 and STR2 are equal, ignoring case."
  (and (stringp str1) (stringp str2)
       (eq t (compare-strings str1 0 nil str2 0 nil t))))

;; id utils
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

;;;; org utils
(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "notion"
			     :follow 'org-notion-browse
			     :complete (lambda (x)
					 (format "notion:%s" x))))

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
    (signal 'org-notion-bad-time iso-time-str)))

;; TODO 2022-01-07: account for timezone
(defun org-notion-from-org-time (org-time-str)
  "Convert ORG-TIME-STR back to ISO-8601 time format."
  (condition-case _
      (format-time-string
       "%FT%T%z"
       (apply 'encode-time
	      (parse-time-string org-time-str))
       t)
    (signal 'org-notion-bad-time org-time-str)))

(defun org-notion-id-at-point (&optional pom)
  "Get the org-notion-id at point. Checks
headline at POM first, then buffer keywords."
  (let ((id (org-notion-field-assoc 'id)))
  (org-with-point-at pom
    (or (thing-at-point 'uuid t)
     (cdr (assoc id (org-entry-properties)))
     (cadr (assoc id (org-collect-keywords (list id))))))))

;;;; Callbacks

(defmacro org-notion-with-callback (&rest body)
  "Evaluate BODY as a callback for `org-notion-dispatch'. Errors are
automatically handled and then `org-notion-last-dispatch-result'
is set to capture the raw sexp value of json-data which can be
further processed by BODY before being returned by
`org-notion-dispatch'."
  (declare (debug t)
	   (indent 0))
  `(lambda (_response) ;; should probably use this
     (with-current-buffer (current-buffer)
       ;;  quick hack. url.el will always return body at this position.
       (search-forward "\n\n")
       (let ((json-data (json-read)))
	 (setq org-notion-last-dispatch-result json-data)
	 (org-notion--handle-http-error json-data)
	 ,@body))))

(defun org-notion-callback-default ()
  "Read json string from `org-notion-dispatch' status buffer and return output"
  (org-notion-with-callback (org-notion-log "%s" json-data)))

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
	   (cons (format "\n|%s|\n" (eieio-object-class-name obj)) (mapcar (lambda (slot)
		      (let ((slot (intern (pp-to-string slot))))
			(format "%+4s:   %s\n" slot (slot-value obj (intern (pp-to-string slot))))))
		    slots)))))

;;;; Requests

(defun org-notion-search-data (query &optional sort filter start_cursor page_size)
  "Prepare data for Notion search request. Return a json object.

QUERY is a string (can be empty).
SORT is either \"ascending\" or \"descending\".
FILTER is either \"page\" or \"database\".
START_CURSOR is an ID and PAGE_SIZE is an integer."
  (let (data)
    (push `(query . ,query) data)
    (when sort
      (if (and (stringp sort)
	       (or (string= sort "ascending")
		   (string= sort "descending")))
	  (nconc data `((sort . ((direction . ,sort) (timestamp . "last_edited_time")))))
	(error "SORT must be either 'ascending' or 'descending'")))
    (when filter
      (if (and (stringp filter)
	       (or (string= filter "page")
		   (string= filter "database"))) 
	  (nconc data `((filter . ((value . ,filter) (property . "object")))))
	(error "FILTER must be either 'page' or 'database'")))
    (when start_cursor
      (if (org-notion-uuid-p start_cursor)
	  (nconc data `((start_cursor . ,start_cursor)))
	(signal 'org-notion-invalid-uuid start_cursor)))
    (when page_size
      (if (and (integerp page_size)
	       (>= org-notion-max-page-size page_size))
	  (nconc data `((page_size . ,page_size)))
	(signal 'org-notion-invalid-page-size page_size)))
    data))

(defun org-notion-database-create-data (parent properties &optional title)
  "Prepare data for Notion create-database request. Return a json object."
  (let (data)
    (push `(parent . ,parent) data)
    (nconc data `((properties . ,properties)))
    (when title
      (if (stringp title)
	  (nconc data `((title . ,title)))
	(error "TITLE must be a string.")))
    data))

(defun org-notion-database-update-data (database_id &optional title properties)
  "Prepare data for Notion update-database request. Return a json object."
  (let (data)
    (push `(database_id . ,database_id) data)
    (when title
      (if (stringp title)
	  (nconc data `((title . ,title)))
	(error "TITLE must be a string.")))
    (when properties)
    data))

(defun org-notion-database-query-data (database_id &optional sorts filter start_cursor page_size)
  "Prepare data for Notion query-database request. Return a json object."
  (let (data)
    (push `(database_id . ,database_id) data)
    (when sorts)
    (when filter)
    (when start_cursor
      (if (org-notion-uuid-p start_cursor)
	  (nconc data `((start_cursor . ,start_cursor)))
	(signal 'org-notion-invalid-uuid start_cursor)))
    (when page_size
      (if (and (integerp page_size)
	       (>= org-notion-max-page-size page_size))
	  (nconc data `((page_size . ,page_size)))
	(signal 'org-notion-invalid-page-size page_size
	 )))
    data))

(defun org-notion-page-data (parent properties &optional children icon cover)
  "Prepare data for Notion create-page request. Return a json object."
  (let (data)
    (push `(parent . ,parent) data)
    (nconc data `((properties . ,properties)))
    (when children)
    (when icon)
    (when cover)
    data))

(defclass org-notion-request-data (org-notion-class)
  ((id
    :initform nil
    :type (or null string)
    :initarg :id
    :documentation "ID associated with this request, if any.")
   (body
    :initform nil
    :type (or null list)
    :initarg :body
    :documentation "JSON body associated with this request, if any.")))

(defun org-notion-block-update-data (obj)
  "Prepare data for Notion update-block request. Return a json object."
  (if (org-notion-block-p obj)
      (org-notion-request-data :id (org-notion-id obj) :body (org-notion-to-json obj))
    (signal 'org-notion-invalid-object obj)))

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
    :initform 'current-user
    :initarg :method
    :type symbol
    :documentation "Notion call symbol. See
    `org-notion-method-types' for possible values.")
   (data
    :initform nil
    :initarg :data
    :type (or null list string org-notion-request-data)
    :documentation "Payload to be sent with HTTP request.")
   (callback
    :initform (org-notion-callback-default)
    :initarg :callback
    :documentation "Callback used to handle response from Notion
    API call.")
   (async
    :initform nil
    :initarg :async
    :type boolean
    :documentation "If t perform request asynchronously. Note that both values will
use `url-retrieve' internally which is async. The only difference
is that when async is t, `org-notion-dispatch' will wait for and
return `org-notion-last-dispatch-result'. When async is nil
`org-notion-dispatch' returns immediately and the
`org-notion-last-dispatch-result' value will be updated by the
`url-retrieve' call."))
  :documentation "Notion API request.")

(cl-defmethod org-notion-dispatch ((obj org-notion-request))
  "Dispatch HTTP request with slots from `org-notion-request' OBJ instance."
  (setq org-notion-last-dispatch-result nil)
  (with-slots (token version endpoint method data callback async) obj
    (let ((url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (funcall token)))
				       ("Notion-Version" . ,version)
				       ("Content-Type" . "application/json")))
	  (callback (or callback (org-notion-callback-default))))
      (pcase method
	('search (let ((url-request-method "POST")
		       (url (concat endpoint "search"))
		       (url-request-data (json-encode data)))
		   (url-retrieve url callback nil nil nil)))
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
	       (url (concat endpoint "databases/" (org-notion-valid-uuid data))))
	   (url-retrieve url callback nil nil nil)))
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
	 ;; (ID . PROP-ID)
	 (let ((url-request-method "GET")
	       (url (concat endpoint (format "pages/%s/properties/%s" (car data) (cdr data)))))
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
	       (url (concat endpoint "blocks/%s" (oref data :id)))
	       (url-request-data (oref data :body)))
	   (url-retrieve url callback nil nil nil)))
	;; children: <blocks>
	;; maximum depth of nesting for each append request is 2 (!!)
	('append-block
	 (let ((url-request-method "PATCH")
	       (url (concat endpoint "blocks/%s/children" (oref data :id)))
	       (url-request-data (oref data :body)))
	   (url-retrieve url callback nil nil nil)))
	('delete-block
	 (let ((url-request-method "DELETE")
	       (url (concat endpoint "blocks/%s" data)))
	   (url-retrieve url callback nil nil nil)))
	(err (signal 'org-notion-invalid-method err)))
      (unless async
	(while (not org-notion-last-dispatch-result)
	  (sleep-for org-notion-dispatch-sync-sleep))
	(when org-notion-last-dispatch-result
	  org-notion-last-dispatch-result)))))

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
	   (org-notion-string= key (org-notion-type obj)))
      (throw 'org-notion-hash-ok 'type))
  (if (and (memq 'parent pred)
	   (org-notion-string= key (org-notion-parent obj)))
      (throw 'org-notion-hash-ok 'parent))
  (if (and (memq 'email pred)
	   (org-notion-string= key (org-notion-email obj)))
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
  (let ((key (org-notion-id obj)))
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
		 added to it."))
   :documentation "Mixin used to cache object instances based on
   `eieio-instance-tracker'. Inheritors must override
   `cache' which is a variable used to cache instances."
   :abstract t)

(cl-defmethod cache-instance ((this org-notion-cache) &rest _slots)
  "Make sure OBJ is in our cache. Optional argument SLOTS are the
initialization arguments. This will not update a duplicate
hash. The old one is always kept.

This function is a no-op if `org-notion-cache-enable' is non-nil.

If `org-notion-cache-overwrite' is non-nil, insert value even if
duplicate is detected."
  (when org-notion-cache-enable
    (let ((table (symbol-value (oref this cache)))
	  (key (org-notion-id this)))
      (if (gethash key table)
	(if org-notion-cache-overwrite
	    (progn
	      (org-notion-log "duplicate key: %s" key)
	      (puthash key this table))
	  (org-notion-log "duplicate key: %s" key)
	  nil)
	(puthash key this table)))))

(cl-defmethod delete-instance ((this org-notion-cache))
  "Remove THIS from cache."
  (let ((id (org-notion-id this)))
  (remhash id (symbol-value (oref this cache)))
  (org-notion-log (format "removed key: %s" id))))

(cl-defmethod update-instance ((this org-notion-cache))
  "Update hash of THIS in cache."
  (puthash (org-notion-id this) this (symbol-value (oref this cache))))

;;;; Object methods

;; The following generic functions are implemented by
;; `org-notion-object' subclasses.

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
    :initform nil
    :initarg :type
    :type (or symbol null)
    :accessor org-notion-type
    :documentation "Type of the user. This slot should be either
    'person' or 'bot'.")
   (name
    :initform nil
    :initarg :name
    :type (or string null)
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
    :accessor org-notion-email
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
  "Interpret JSON as `org-notion-user'."
  (if (string= "user" (alist-get 'object json))
      (progn
	(oset-and
	 obj
	 :id (alist-get 'id json)
	 :type (if-let ((type (alist-get 'type json)))
		   (intern type) nil)
	 :name (alist-get 'name json)        
	 :avatar (alist-get 'avatar_url json)
	 :email (alist-get 'email json)      
	 :owner (cdadar (alist-get 'bot json)))
	(cache-instance obj))
    (error "expected user object, found %s" (alist-get 'object json)))
  obj)

(cl-defmethod org-notion-to-json ((obj org-notion-user))
  (with-slots (id type name avatar email owner) obj
    (list (cons 'object "user") (cons 'id id)
	  (cons 'name name) (cons 'avatar_url avatar)
	  (cons 'type (symbol-name type)))))

(cl-defmethod org-notion-to-org ((obj org-notion-user) &optional type)
  "Convert user to org-element TYPE."
  (with-slots (id usr-type name avatar email owner) obj
    (let* ((usr-str (concat name " " (when email (format "<%s>" email))))
	   (props
	    (unless (not '(id avatar email))
	      (org-notion--property-drawer
	       `((id . ,id)
		 (email . ,email)
		 (avatar . ,avatar))))))
      (pcase type
	((or 'nil 'heading)
	 (org-element-interpret-data
	  `(headline
	    (:title ,name :level 1)
	    ,props)))
	('kw (org-element-interpret-data (org-notion--prop 'user usr-str 'kw)))
	('prop (org-element-interpret-data
		(org-notion--prop 'user usr-str 'prop)))
	(_ (signal 'org-notion-invalid-element-type type))))))

(cl-defmethod org-notion-from-org ((obj org-notion-user) &optional str)
  "Parse STR org element into an `org-notion-user' OBJ."
  (with-slots (id usr-type name avatar email owner) obj
    (with-temp-buffer
      (insert str)
      (let* ((elt (caddr (org-element-parse-buffer)))
	     (type (car elt))
	     (map-key nil))
	(fset 'map-key (lambda (k v)
			(pcase k
			  ("NOTION_USER" (setq name v))
			  ("NOTION_ID" (setq id v))
			  ("NOTION_EMAIL" (setq email v))
			  ("NOTION_AVATAR_URL" (setq avatar v)))))
	(pcase type
	  ('headline
	   (let* ((plst (cadr elt))
		  (p-usr (or (plist-get plst :NOTION_USER) (plist-get plst :title)))
		  (p-id (plist-get plst :NOTION_ID))
		  (p-email (plist-get plst :NOTION_EMAIL))
		  (p-avatar (plist-get plst :NOTION_AVATAR_URL)))
	     (setq name p-usr
		   id p-id
		   email p-email
		   avatar p-avatar)))
	  ('keyword
	   (let* ((plst (cadr elt))
		  (key (plist-get plst :key))
		  (val (plist-get plst :value)))
	     (map-key key val)))
	  ('section
	   (let ((keywords (nthcdr 2 elt)))
	     (dolist (kw keywords)
	       (if (eq (car kw) 'keyword)
		   (let* ((plst (cadr kw))
			  (key (plist-get plst :key))
			  (val (plist-get plst :value)))
		     (message "%s %s" key val)
		     (map-key key val))))))
	  (_ (signal 'org-notion-invalid-element-type type)))
	(cache-instance obj)
	obj))))

;;;;; Property Item

(defclass org-notion-property-item (org-notion-object)
  ((type
    :initform nil
    :initarg :type
    :accessor org-notion-type
    :type (or symbol null)
    :documentation "Type of the properties. See variable `org-notion-property-types' for possible values."))
  :documentation "Notion property_item object - identified by the `:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-property-item) json)
    "Interpret JSON as `org-notion-property-item'.")

(cl-defmethod org-notion-to-json ((obj org-notion-property-item))
  "Interpret `org-notion-property-item' as JSON.")

(cl-defmethod org-notion-from-org ((obj org-notion-property-item) str))

(cl-defmethod org-notion-to-org ((obj org-notion-property-item) &optional type))

;;;;; Parent Object

(defclass org-notion-parent-obj (org-notion-object)
  ((type
    :initform nil
    :initarg :type
    :accessor org-notion-type
    :type (or symbol null)
    :documentation "Type of parent object. See variable `org-notion-parent-types' for possible values."))
  :documentation "Notion parent object - identified by the `:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-parent-obj) json)
  "Interpret JSON as `org-notion-parent-obj'."
  (setf json (or (alist-get 'parent json) json))
  (let* ((type (alist-get 'type json))
	 (type_id (intern type))
	 (id (alist-get type_id json)))
    (oset-and obj :type (intern (if (string= type "workspace") type (substring type 0 -3))) :id id)
    obj))

(cl-defmethod org-notion-to-json ((obj org-notion-parent-obj))
  (with-slots (id type) obj
    (let ((type (concat (symbol-name type) "_id")))
    `(parent (type . ,type) (,(intern type) . ,id)))))

;; TODO 2023-01-15
;; (cl-defmethod org-notion-from-org ((obj (subclass org-notion-rich-text)) str))
;; (cl-defmethod org-notion-to-org ((obj (subclass org-notion-rich-text)) &optional type))

;;;;; Database

(defclass org-notion-database (org-notion-object)
  ((title
    :initform nil
    :initarg :title
    :type (or string null)
    :documentation "Name of the database as it appears in
    Notion.")
   (created_by
    :initform nil
    :initarg :created_by
    :type (or org-notion-user null)
    :documentation "User who created this block.")
   (created_time
    :initform (format-time-string "%FT%T%z")
    :initarg :created_time
    :documentation "Datetime when this block was created.")
   (last_edited_by
    :initform nil
    :initarg :last_edited_by
    :type (or org-notion-user null)
    :documentation "Last user to edit this block.")
   (last_edited_time
    :initform (format-time-string "%FT%T%z")
    :initarg :last_edited_time
    :documentation "Datetime when this block was last edited.")
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
    :initform nil
    :accessor org-notion-properties
    :documentation "Schema of properties for the database as they
    appear in Notion.")
   (parent
    :initform nil
    :initarg :parent
    :accessor org-notion-parent
    :type (or list null)
    :documentation "The parent of this database. Value is a cons cell (TYPE . ID).")
   (url
    :initform nil
    :initarg :url
    :type (or null string)
    :accessor org-notion-url
    :documentation "The URL of the Notion database."))
  :documentation "Notion database object - identified by the
`:id' slot.")

(cl-defmethod org-notion-from-json ((obj org-notion-database) json)
  "Interpret JSON as `org-notion-database'."
  ;; quick check
  (if (string= (alist-get 'object json) "database")
      (progn
	(oset-and
	 obj
	 :id (alist-get 'id json)
	 :title (cdr			; down the rabbit-hole we go
		 (cadr
		  (cadr
		   (mapcan
		    (lambda (x)
		      (if (listp x) x nil))
		    (alist-get 'title json))))) ; this is a vector
	 :created_by (alist-get 'created_by json)
	 :created_time (alist-get 'created_time json)
	 :last_edited_by (alist-get 'last_edited_by json)
	 :last_edited_time (alist-get 'last_edited_time json)
	 :icon (alist-get 'icon json)
	 :cover (alist-get 'cover json)
	 :properties (alist-get 'properties json)
	 :parent (org-notion-parse-parent json)
	 :url (alist-get 'url json))
	(cache-instance obj))
    (error "expected database object, found %s" (alist-get 'object json)))
  obj)

(cl-defmethod org-notion-to-json ((obj org-notion-database))
  "Convert database to json."
  (with-slots (id title created_by created_time last_edited_by last_edited_time icon cover properties parent url) obj
    (list (cons 'object "database") (cons 'id id)
	  (cons 'title title) (cons 'created created_time)
	  (cons 'updated last_edited_time) (cons 'icon icon)
	  (cons 'cover cover) (cons 'properties properties)
	  (cons 'parent parent) (cons 'url url))))

(cl-defmethod org-notion-to-org ((obj org-notion-database) &optional type)
  "Convert database to org-element TYPE."
  (with-slots (id title created_by created_time last_edited_by last_edited_time icon cover properties parent url) obj
    ;; TODO 2023-01-07: we can do better here
    (let ((props
	   (when (or id icon cover created_time last_edited_time url)
	     (org-notion--property-drawer
	      `((id . ,id)
	       (icon . ,icon)
	       (cover . ,cover)
	       (created . ,created_time)
	       (updated . ,last_edited_time)
	       (url . ,url))))))
      (pcase type
	((or 'nil 'heading)
	 (org-element-interpret-data
	  `(headline
	    (:title ,title :level 1)
	    ,props)))
	('kw (org-element-interpret-data (org-notion--prop 'id id 'kw)))
	('prop (org-element-interpret-data
		(org-notion--prop 'id id 'prop)))
	;; TODO
	('table nil)
	(_ (error 'org-notion-invalid-element-type type))))))

(cl-defmethod org-notion-from-org ((obj org-notion-database) &optional str)
  "Parse STR org element into an `org-notion-database' OBJ."
  (with-slots (id title created updated icon cover properties parent url) obj
    (with-temp-buffer
      (insert str)
      (let* ((elt (caddr (org-element-parse-buffer)))
	     (type (car elt))
	     (map-key nil))
	(fset 'map-key (lambda (k v)
	   (pcase k
	     ("NOTION_ID" (setq id v))
	     ("NOTION_URL" (setq url v))
	     ("NOTION_ICON" (setq icon v))
	     ("NOTION_COVER" (setq cover v)))))
	(pcase type
	  ('headline
	   (let* ((plst (cadr elt))
		  (p-id (plist-get plst :NOTION_ID))
		  (p-icon (plist-get plst :NOTION_ICON_URL))
		  (p-cover (plist-get plst :NOTION_COVER_URL))
		  (p-created (plist-get plst :CREATED))
		  (p-updated (plist-get plst :UPDATED))
		  (p-url (plist-get plst :NOTION_URL)))
	     (setq id p-id
		   icon p-icon
		   cover p-cover
		   created p-created
		   updated p-updated
		   url p-url)))
	  ('keyword
	   (let* ((plst (cadr elt))
		  (key (plist-get plst :key))
		  (val (plist-get plst :value)))
	     (map-key key val)))
	  ('section
	   (let ((keywords (nthcdr 2 elt)))
	     (dolist (kw keywords)
	       (if (eq (car kw) 'keyword)
		   (let* ((plst (cadr kw))
			  (key (plist-get plst :key))
			  (val (plist-get plst :value)))
		     (org-notion-log "%s" key val)
		     (map-key key val))))))
	  (_ (error 'org-notion-invalid-element-type type)))
	(cache-instance obj)
	obj))))

;;;;; Page

(defclass org-notion-page (org-notion-object)
  (
   (created_by
    :initform nil
    :initarg :created_by
    :type (or org-notion-user null)
    :documentation "User who created this block.")
   (created_time
    :initarg :created_time
    :documentation "Datetime when this block was created.")
   (last_edited_by
    :initform nil
    :initarg :last_edited_by
    :type (or org-notion-user null)
    :documentation "Last user to edit this block.")
   (last_edited_time
    :initarg :last_edited_time
    :documentation "Datetime when this block was last edited.")
   (archived
    :initform nil
    :initarg :archived
    :type boolean
    :documentation "The archived status of the page.")
   (icon
    :initform nil
    :initarg :icon
    :type (or null string list)
    :documentation "Page icon.")
   (cover
    :initform nil
    :initarg :cover
    :type (or null string list)
    :documentation "Page cover image.")
   (properties
    :initarg :properties
    :accessor org-notion-properties
    :type (or list null)
    :documentation "Property values of this page.")
   (parent
    :initform nil
    :initarg :parent
    :accessor org-notion-parent
    :type (or list null)
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
  "Interpret JSON as `org-notion-page'."
  (if (string= "page" (alist-get 'object json))
      (progn
	(oset obj :id (alist-get 'id json))
	(oset obj :created_by (alist-get 'created_by json))
	(oset obj :created_time (alist-get 'created_time json))
	(oset obj :last_edited_by (alist-get 'last_edited_by json))
	(oset obj :last_edited_time (alist-get 'last_edited_time json))
	(oset obj :archived (org-notion--get-flag 'archived json))
	(oset obj :icon (alist-get 'icon json))
	(oset obj :cover (alist-get 'cover json))
	(oset obj :parent (org-notion-parse-parent json))
	(oset obj :properties (alist-get 'properties json))
	(oset obj :url (alist-get 'url json))
	(cache-instance obj))
    (error "expected page object, found %s" (alist-get 'object json)))
  obj)

;; (cl-defmethod org-notion-from-org ((obj org-notion-page) str))

(cl-defmethod org-notion-to-org ((obj org-notion-page) &optional type)
  (with-slots (id created updated properties parent archived icon cover url) obj
    (pcase type
      ((or 'nil 'headline)
       (let ((title
	      (alist-get 'content
			 (alist-get 'text (aref (alist-get 'title (alist-get 'Name properties)) 0)))))
	 (org-element-interpret-data
	  `(headline
	    (:title ,title :level 1	; FIXME
		    :CREATED (org-notion-to-org-time created)
		    :UPDATED (org-notion-to-org-time updated))
	    (property-drawer
	     nil ((node-property (:key "NOTION_ID" :value ,id))
		  (node-property (:key "NOTION_URL" :value ,url))
		  (node-property (:key "NOTION_ICON" :value ,icon))
		  (node-property (:key "NOTION_COVER" :value ,cover))
		  (node-property (:key "NOTION_PARENT" :value ,parent))
		  (node-property (:key "CREATED" :value ,created))
		  (node-property (:key "UPDATED" :value ,updated))		
		  (when archived
		    (node-property (:key "NOTION_ARCHIVED" :value t)))))))))
      ('file
       (org-element-interpret-data
	`(org-data nil (section nil)
		   (keyword (:key "NOTION_ID" :value ,id))
		   (keyword (:key "NOTION_URL" :value ,url))
		   (keyword (:key "NOTION_ICON" :value ,icon))
		   (keyword (:key "NOTION_COVER" :value ,cover))
		   (keyword (:key "NOTION_PARENT" :value ,parent))
		   (keyword (:key "CREATED" :value ,created))
		   (keyword (:key "UPDATED" :value ,updated))
		   (when archived (keyword (:key "NOTION_ARCHIVED" :value ,archived))))))
      (_ (signal 'org-notion-invalid-element-type type)))))

;;;;; Block

(defclass org-notion-block (org-notion-object)
  ((type
    :initform 'unsupported
    :initarg :type
    :accessor org-notion-type
    :type symbol
    :documentation "Type of block. See variable
    `org-notion-block-types' for possible values.")
   (created_by
    :initform nil
    :initarg :created_by
    :type (or org-notion-user null)
    :documentation "User who created this block.")
   (created_time
    :initarg :created_time
    :documentation "Datetime when this block was created.")
   (last_edited_by
    :initform nil
    :initarg :last_edited_by
    :type (or org-notion-user null)
    :documentation "Last user to edit this block.")
   (last_edited_time
    :initarg :last_edited_time
    :documentation "Datetime when this block was last edited.")
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
  "Interpret JSON as `org-notion-block'."
  (if (string= "block" (alist-get 'object json))
      (progn
	;; capture early block data from json
	(oset obj :id (alist-get 'id json))
	(oset obj :type (intern (alist-get 'type json)))
	(oset obj :created_by (alist-get 'created_by json))
	(oset obj :created_time (alist-get 'created_time json))
	(oset obj :last_edited_by (alist-get 'last_edited_by json))
	(oset obj :last_edited_time (alist-get 'last_edited_time json))
	(oset obj :archived (org-notion--get-flag 'archived json))
	(oset obj :has_children (org-notion--get-flag 'has_children json))
	;; use value of :type slot to capture text, children, and
	;; properties.
	(pcase (org-notion-type obj)
	  ('paragraph
	   (let* ((content (alist-get 'paragraph json))
		  ;; TODO 2023-01-01: this only accounts for a single
		  ;; text object in the array.
		  (val (elt (alist-get 'text content) 0))
		  (txt (org-notion-rich-text
			:type (intern (alist-get 'type val))
			:color (intern (alist-get 'color content))
			:annotations (alist-get 'annotations val)
			:plain_text (alist-get 'plain_text val)
			:href (alist-get 'href val))))
	     (oset obj :text (vector txt))))
	  ('heading_2 ())		; text
	  ('heading_3 ())		; text
	  ('bulleted_list_item ())	; text + children
	  ('numbered_list_item ())	; text + children
	  ('to_do ())			; text + children + properties
	  ('toggle ())			; text + children
	  ('child_page			; properties
	   (oset obj :properties (car (alist-get 'child_page json))))
	  ('child_database		; properties
	   (oset obj :properties (car (alist-get 'child_database json))))
	  ('embed			; properties
	   (oset obj :properties (car (alist-get 'embed json))))
	  ('image ()) ; file https://developers.notion.com/reference/file-object
	  ('video ()) ; file
	  ('file ())  ; file + text
	  ('pdf ())   ; file
	  ('bookmark ())   ; text + properties
	  ('callout ())	   ; text + properties + children + file/emoji
	  ('quote ())	   ; text + children
	  ('code ())	   ; text + properties
	  ('equation ())   ; properties
	  ('divider ())	   ; nil
	  ('table_of_contents ())	; nil
	  ('breadcrumb ())		; nil
	  ('column ())			; children
	  ('column_list ())		; children
	  ('link_preview ())		; properties
	  ('synced_block ())		; properties + children
	  ('template ())		; text + children
	  ('link_to_page ())		; properties
	  ('table ())			; properties
	  ('table_row ())		; children
	  ('unsupported ())
	  (_ ())))
    (error "expected block object, found %s" (alist-get 'object json)))
  (cache-instance obj)
  obj)

;; (cl-defmethod org-notion-to-json ((obj org-notion-block)))
;; (cl-defmethod org-notion-to-org ((obj org-notion-block) &optional _))

;;;;; Text

;; Parent class for Notion Rich-text objects. Instances of subclasses
;; are de/serialized as an array of JSON objects for interaction with
;; the Notion API and as Org syntax.
(defclass org-notion-rich-text (org-notion-class)
  ((type
    :initform 'text
    :initarg :type
    :accessor org-notion-type
    :type symbol
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
    :type (or null list symbol)
    :documentation "All annotations that apply to this rich
    text. See `org-notion-annotation-types' for a list of
    possible values.")
   (color
    :initform 'default
    :initarg :color
    :type symbol
    :documentation "Color that applies to this rich text. See
    `org-notion-color-types' for a list of possible values."))
  :documentation "Notion rich text object.")

;; (cl-defmethod org-notion-from-json ((obj (subclass org-notion-rich-text))))
;; (cl-defmethod org-notion-to-json ((obj (subclass org-notion-rich-text))))
;; (cl-defmethod org-notion-from-org ((obj (subclass org-notion-rich-text)) str))
;; (cl-defmethod org-notion-to-org ((obj (subclass org-notion-rich-text)) &optional type))

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

;;; API Calls

;;;###autoload
(defun org-notion-get-current-user ()
  "Retrieve the bot user associated with the current
`org-notion-token'. This function sets and returns the
`org-notion-current-user' variable."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'current-user
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "user")
		  (setq org-notion-current-user
			(org-notion-from-json (org-notion-user) json-data))
		  (org-notion-dbg json-data))
		json-data)))
  org-notion-current-user)

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
			(org-notion-print (org-notion-from-json (org-notion-user) i))
			)))))))

;;;###autoload
(defun org-notion-get-user (id)
  "Get user with given ID from workspace."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'user
    :data id
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "user")
		  (org-notion-from-json (org-notion-user) json-data))))))

;;;###autoload
(defun org-notion-get-database (id)
  "Get database with given ID from workspace."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'database
    :data id
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "database")
		  (org-notion-from-json (org-notion-database) json-data))))))

;; TODO 2023-01-08
;;;###autoload
(defun org-notion-query-database (id query)
  "Query database with ID using QUERY."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'database
    :data (cons id query)
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "list")
		  json-data)))))

;; (defun org-notion-create-database () )
;; (defun org-notion-update-database () )

;;;###autoload
(defun org-notion-get-page (id)
  "Get page with given ID from workspace."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'page
    :data id
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "page")
		  (org-notion-from-json (org-notion-page) json-data))))))

(defun org-notion-get-page-property (id prop-id)
  "Get PROP-ID property for page with given ID from workspace."
  (interactive)
  (org-notion--get-results
   (org-notion-dispatch
    (org-notion-request
     :method 'page-property
     :data (cons id prop-id)))))

;; (defun org-notion-create-page () )
;; (defun org-notion-update-page () )

;;;###autoload
(defun org-notion-get-block (id)
  "Get block with given ID from workspace."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'block
    :data id
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "block")
		  (org-notion-from-json (org-notion-block) json-data))))))

;; TODO 2023-01-08
;;;###autoload
(defun org-notion-get-block-children (id)
  "Get children from block with given ID from workspace."
  (interactive)
  (org-notion-dispatch
   (org-notion-request
    :method 'block-children
    :data id
    :callback (org-notion-with-callback
		(when (equal (cdar json-data) "list")
		  json-data)))))

;; (defun org-notion-append-block () )
;; (defun org-notion-delete-block () )

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
		     (org-notion-log "%s" results)
		     (setq org-notion-last-dispatch-result results)))))))

;;;; Org Parsers

;; TODO 2023-01-10
(defun org-notion-parse-buffer (&optional target visible-only)
  "Recursively parse the buffer TARGET and return an
 `org-notion-object'. Defaults to current buffer.

When VISIBLE-ONLY is non-nil, don’t parse contents of hidden
elements."
  (interactive)
  (with-current-buffer (or target (current-buffer))
    (let ((tree (org-element-parse-buffer nil visible-only)))
      tree)))

(defun org-notion-element-context (&optional class)
  "Parse STR as `org-notion-object' TYPE.

TYPE is a symbol and an alist key in `org-notion-class-names'."
  (org-notion-class-assoc class))

;;;; Commands

;;;###autoload
(defun org-notion-browse (&optional uuid)
  "Open a Notion page by UUID."
  (interactive "sID: ")
  (if-let ((id (or uuid (org-notion-id-at-point))))
      (browse-url (format "https://www.notion.so/%s" id))
    (message "failed to find %s" (org-notion-field-assoc 'id))))

;; ;;;###autoload
;; (defun org-notion-push (&optional subtree)
;;   "Push current buffer to Notion.so. If SUBTREE is non-nil, limit to
;; the current heading."
;;   (interactive)
;;   (with-current-buffer (current-buffer)
;;     (let ((tree (if subtree
;; 		    (progn (org-back-to-heading t)
;; 			   (org-notion-parse-element 'heading))
;; 		  (org-notion-parse-buffer)))))))

;; ;;;###autoload
;; (defun org-notion-pull (&optional buf)
;;   "Pull and update the current headline from Notion. If BUF is
;; non-nil pull updates for entire buffer."
;;   (interactive))

;;; Minor-mode
;; 
(defun org-notion--kbd (key)
  "Convert KEY to internal Emacs key representation with `org-notion-keymap-prefix'"
  (kbd (concat org-notion-keymap-prefix " " key)))

(define-minor-mode org-notion-mode
  "Toggle org-notion minor-mode."
  :global nil
  :group 'org-notion
  :lighter " notion"
  :keymap
  (list	(cons (org-notion--kbd "p") #'org-notion-push)
	(cons (org-notion--kbd "f") #'org-notion-pull)
	(cons (org-notion--kbd "o") #'org-notion-browse)
	(cons (org-notion--kbd "s") #'org-notion-search)
	(cons (org-notion--kbd "r u") #'org-notion-get-users)))

(provide 'org-notion)
;;; org-notion.el ends here
