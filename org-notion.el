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
  "FQDN of Notion API. This is used to create an `auth-source' entry.")

(defconst org-notion-endpoint (format "https://%s/v1/" org-notion-host)
  "URL of Notion API endpoint")

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

(defvar org-notion-token nil
  "Notion integration token used to authenticate requests.
You can generate one at URL `https://www.notion.so/my-integrations'.")

(defvar org-notion-page-size 100
  "Default count of results returned by Notion. Additional calls
will be made to collect all results using the 'start_cursor' parameter. Maximum value is 100.")

;; RESEARCH 2021-12-28: auth-source secret const function security reccs
(defun org-notion-find-token (&optional token)
  "Provision VAR `org-notion-token'.
If `org-notion-use-auth-source' is 't' check auth-source
first. If 'nil' or token is missing, prompt for token."
  (when (and org-notion-use-auth-source
	     (not org-notion-token))
    ;; TODO 2021-12-28: add default user/host/port with let*
      (let ((found (nth 0 (auth-source-search
			   :max 1
			   :host org-notion-host
			   :require '(:secret)
			   :create t))))
	(setq org-notion-token (when found
				 (let ((sec (plist-get found :secret)))
				   (if (functionp sec)
				       (funcall sec)
				     sec))))))
  (unless org-notion-use-auth-source
    (setq org-notion-token (or token (read-passwd "Notion API Token: "))))
  org-notion-token)
  
(defun org-notion-get-current-user ()
  "Retrieve the bot user associated with the current
`org-notion-token'"
  (interactive)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-find-token)))
	   ("Notion-Version" . ,org-notion-version))))
    (url-retrieve (concat org-notion-endpoint "users/me") (lambda (_status) (switch-to-buffer (current-buffer))))))

(defun org-notion-get-users ()
  "Get all users in the Notion workspace. This will return a 403
status code if your integration doesn't have User Capabilities
enabled."
  (interactive)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-find-token)))
	   ("Notion-Version" . ,org-notion-version))))
    (url-retrieve (concat org-notion-endpoint "users") (lambda (_status) (switch-to-buffer (current-buffer))))))

(defun org-notion-search (query &optional params)
  "Search the Notion workspace using QUERY and optional PARAMS"
  (interactive)
  (let ((query query)
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " (org-notion-find-token)))
	   ("Notion-Version" . ,org-notion-version)))
	(url-request-data (json-encode (list :query query))))
    (url-retrieve (concat org-notion-endpoint "search") (lambda (_status) (with-current-buffer (current-buffer) (message "success"))))))

(provide 'org-notion)
;;; org-notion.el ends here
