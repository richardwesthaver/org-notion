;;; org-notion-test.el --- tests for org-notion -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021  ellis
;;
;; Author: ellis <ellis@zor>
;; Keywords: tools
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
;; tests for org-notion.el
;;
;;; Code:
(require 'org-notion (expand-file-name "../org-notion"))
(require 'ert)

(defvar org-notion-load-utils nil)
(when org-notion-load-utils (require 'org-notion-utils "org-notion-utils"))

(defun org-notion--get-results (json)
  (when (equal (cdar json) "list")
    (let ((results (alist-get 'results json)))
      (dolist (i (append results nil)))
      results)))

(defun org-notion--get-children (json))

(defvar org-notion--mock-file (expand-file-name "mock.org"))

(defvar org-notion--mock-data
  (with-current-buffer (find-file-noselect org-notion--mock-file)
    (org-element-parse-buffer)))

(ert-deftest consts-ok ()
  (should (equal org-notion-version "2021-08-16"))
  (should (equal org-notion-host "api.notion.com"))
  (should (equal org-notion-endpoint "https://api.notion.com/v1/")))

(ert-deftest no-auth-source-ok ()
  (let ((org-notion-use-auth-source nil))
    (should (equal (org-notion-token "token-test") "token-test"))))

;; FIXME 2022-01-02: currently requires ~/.authinfo.gpg
(ert-deftest auth-source-ok ()
  (let ((org-notion-use-auth-source t))
    (should (org-notion-token))))

(ert-deftest get-current-user-ok () (should (org-notion-get-current-user)))

(ert-deftest get-users-ok () (should (org-notion-get-users)))

(ert-deftest user-from-json-ok ()
  (let ((results (org-notion--get-results (json-read-file "json/users.json"))))
    (org-notion-from-json (org-notion-user) (elt results 0))
    (should (org-notion-from-json (org-notion-user) (elt results 0)))))

(ert-deftest user-to-json-ok ()
  (should (org-notion-to-json (org-notion-user))))

(ert-deftest user-to-org-ok ()
  (should (org-notion-to-org (org-notion-user :id "75d6ae70-302e-2e5f-00d9-8229dfb658c9") 'heading))
  (should (org-notion-to-org (org-notion-user :id "524df0d1-a5ee-c70c-bb2c-7a863ce50135") 'property))
  (should (org-notion-to-org (org-notion-user :id "b41423bc-9025-141c-8eca-469652cc524c") 'keyword)))

;; TODO 2022-12-27
(ert-deftest user-from-org-ok ()
  (let ((data org-notion--mock-data))
    ))

(ert-deftest db-from-json-ok ()
  (let ((db (json-read-file "json/database.json")))
    (should (org-notion-from-json (org-notion-database) db))))

(ert-deftest db-to-json-ok ()
  (should (org-notion-to-json (org-notion-database))))

(ert-deftest db-to-org-ok ()
  (should (org-notion-to-org (org-notion-database) 'heading)))

;; TODO 2022-12-27
(ert-deftest db-from-org-ok ())

(ert-deftest page-from-json-ok ())
(ert-deftest page-to-json-ok ())
(ert-deftest page-from-org-ok ())
(ert-deftest page-to-org-ok ())

(ert-deftest block-from-json-ok ())
(ert-deftest block-to-json-ok ())
(ert-deftest block-from-org-ok ())
(ert-deftest block-to-org-ok ())

(ert-deftest rich-text-from-json-ok ())
(ert-deftest rich-text-to-json-ok ())
(ert-deftest rich-text-from-org-ok ())
(ert-deftest rich-text-to-org-ok ())

(ert-deftest inline-text-from-json-ok ())
(ert-deftest inline-text-to-json-ok ())
(ert-deftest inline-text-from-org-ok ())
(ert-deftest inline-text-to-org-ok ())

(ert-deftest inline-mention-from-json-ok ())
(ert-deftest inline-mention-to-json-ok ())
(ert-deftest inline-mention-from-org-ok ())
(ert-deftest inline-mention-to-org-ok ())

(ert-deftest inline-equation-from-json-ok ())
(ert-deftest inline-equation-to-json-ok ())
(ert-deftest inline-equation-from-org-ok ())
(ert-deftest inline-equation-to-org-ok ())

(ert-deftest search-ok () (should (org-notion-search "org-notion")))

;; org-mode tests
(ert-deftest to-org-time-ok ()
  (set-time-zone-rule t)
  (should (string= (org-notion-to-org-time "2022-01-09T08:59:15.000Z") "2022-01-09 08:59:15")))

(ert-deftest from-org-time-ok ()
  (set-time-zone-rule t)
  (should (string= (org-notion-from-org-time "2022-01-09 08:59:15") "2022-01-09T08:59:15+0000")))

(ert-deftest id-at-point-ok ()
  (with-temp-buffer
    (insert-file-contents "mock.org")
    (let ((id "64adb50d17394203a31265641aeaeb8e")
	  (pom (point-min)))
      (should (equal (org-notion-id-at-point pom) id)))))

(provide 'org-notion-test)
;;; org-notion-test.el ends here
