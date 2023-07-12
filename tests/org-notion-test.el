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

;; prefer to conditionally enable caching when needed
(setq-local org-notion-cache-overwrite t)
(defvar org-notion-load-utils t)
(when org-notion-load-utils (require 'org-notion-utils (expand-file-name "org-notion-utils")))

(defun org-notion--get-results (json)
  (when (equal (cdar json) "list")
    (let ((results (alist-get 'results json)))
      (dolist (i (append results nil)))
      results)))

;; (defun org-notion--get-children (json))

(defvar org-notion--mock-file (expand-file-name "mock.org"))

(defvar org-notion--mock-data
  (with-current-buffer (find-file-noselect org-notion--mock-file)
    (buffer-substring (point-min) (point-max))))

(defmacro should= (a b &optional test)
  "(should (equal/TEST A B))"
  `(should (,(or test 'equal) ,a ,b)))

(ert-deftest no-auth-source-ok ()
  (let ((org-notion-use-auth-source nil))
    (should= (org-notion-token "token-test") "token-test")))

;; FIXME 2022-01-02: currently requires ~/.authinfo.gpg
(ert-deftest auth-source-ok ()
  (let ((org-notion-use-auth-source t))
    (should (org-notion-token)))
  (should (org-notion-token)))

(ert-deftest cache-overwrite-ok ()
  (let ((org-notion-cache-overwrite t))
    (should (cache-instance (org-notion-user)))
    (setf org-notion-cache-overwrite nil)
    (should= nil (cache-instance (org-notion-page)))))

(ert-deftest cache-enable-ok ()
  (let ((org-notion-cache-enable t))
    (org-notion-clear-cache)
    (should (cache-instance (org-notion-database :id "c872ca0c-fdfe-db54-a4df-b9168de9fa52")))
    (setf org-notion-cache-enable nil)
    (should= nil (cache-instance (org-notion-database :id "c872ca0c-fdfe-db54-a4df-b9168de9fa52")))))

(ert-deftest get-current-user-ok () (should (org-notion-get-current-user)))

(ert-deftest get-users-ok () (should (org-notion-get-users)))

(ert-deftest user-from-json-ok ()
  (let ((results (org-notion--get-results (json-read-file (expand-file-name"json/users.json")))))
    (org-notion-from-json (org-notion-user) (elt results 0))
    (should (org-notion-from-json (org-notion-user :id "140351d1-1562-dc48-80b5-f933e95b8cba") (elt results 0)))))

(ert-deftest user-to-json-ok ()
  (should (org-notion-to-json (org-notion-user :id "bfa671d0-2555-0b10-bd09-6bff9d9153df"))))

(ert-deftest user-to-org-ok ()
  (should (org-notion-to-org (org-notion-user :name "miss mischief" :id "75d6ae70-302e-2e5f-00d9-8229dfb658c9") 'heading))
  (should (org-notion-to-org (org-notion-user :name "dude man" :id "524df0d1-a5ee-c70c-bb2c-7a863ce50135") 'prop))
  (should (org-notion-to-org (org-notion-user :name "some guy" :id "b41423bc-9025-141c-8eca-469652cc524c") 'kw)))

;; TODO 2022-12-27
(ert-deftest user-from-org-ok ()
    (should (org-notion-from-org (org-notion-user) org-notion--mock-data)))

(ert-deftest db-from-json-ok ()
  (let ((db (json-read-file (expand-file-name"json/database.json"))))
    (should (org-notion-from-json (org-notion-database) db))))

(ert-deftest db-to-json-ok ()
  (should (org-notion-to-json (org-notion-database))))

(ert-deftest db-to-org-ok ()
  (should (org-notion-to-org (org-notion-database) 'heading))
  (should (org-notion-to-org (org-notion-database) 'kw))
  (should (org-notion-to-org (org-notion-database) 'prop)))

;; TODO 2022-12-27
(ert-deftest db-from-org-ok ())

(ert-deftest parent-obj-json-ok ()
  (let ((parent '(parent (type . "database_id") (database_id . "bfafc302-c639-447b-ab32-d5bdabf2ba0c"))))
    (should= parent (org-notion-to-json (org-notion-from-json (org-notion-parent-obj) parent)))))

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

(ert-deftest search-ok ()
  (let ((q "org-notion"))
    (should (org-notion-search q "ascending"))
    (should (org-notion-search q "descending" "page"))
    (should (org-notion-search "" nil "database"))))

;; org-mode tests
(ert-deftest to-org-time-ok ()
  (set-time-zone-rule t)
  (should (string= (org-notion-to-org-time "2022-01-09T08:59:15.000Z") "2022-01-09 08:59:15")))

(ert-deftest from-org-time-ok ()
  (set-time-zone-rule t)
  (should (string= (org-notion-from-org-time "2022-01-09 08:59:15") "2022-01-09T08:59:15+0000")))

(ert-deftest id-at-point-ok ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "mock.org"))
    (let ((id "64adb50d17394203a31265641aeaeb8e")
	  (pom (point-min)))
      (should (equal (org-notion-id-at-point pom) id)))))

(provide 'org-notion-test)
;;; org-notion-test.el ends here
