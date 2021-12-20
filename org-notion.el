;;; org-notion.el --- Notion.so Org-mode extension  -*- lexical-binding: t; -*-
;; 
;; Copyright (C) 2021  ellis
;; 
;; Author: ellis <ellis@rwest.io>
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
;; 
;;; Code:
(require 'url)
(require 'json)

(defconst org-notion-endpoint "https://api.notion.com/v1/"
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

;; TODO 2021-12-19: check `org-notion-use-auth-source', if 't' query
;; for token, elif 'nil' or token missing, prompt user for input.
(defun org-notion-find-token ()
  "Provision VAR `org-notion-token'.
If `org-notion-use-auth-source' is 't' check auth-source
first. If 'nil' or token is missing, prompt for token."
  (interactive)
  (setq org-notion-token (read-passwd "Notion API Token: ")))
  
(defun org-notion-get-current-user ()
  "Retrieve the bot user associated with the current
`org-notion-token'"
  (interactive)
  (unless org-notion-token
    (org-notion-find-token))
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " org-notion-token))
	   ("Notion-Version" . ,org-notion-version))))
    (url-retrieve (concat org-notion-endpoint "users/me") (lambda (status) (switch-to-buffer (current-buffer))))))

(defun org-notion-get-users ()
  "Get all users in the Notion workspace. This will return a 403
status code if your integration doesn't have User Capabilities
enabled."
  (interactive)
  (unless org-notion-token
    (org-notion-find-token))
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " org-notion-token))
	   ("Notion-Version" . ,org-notion-version))))
    (url-retrieve (concat org-notion-endpoint "users") (lambda (status) (switch-to-buffer (current-buffer))))))

(provide 'org-notion)
;;; org-notion.el ends here
