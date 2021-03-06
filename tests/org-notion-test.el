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
(require 'org-notion (expand-file-name "../org-notion.el"))
(require 'ert)

(ert-deftest org-notion-consts-ok ()
  (should (equal org-notion-version "2021-08-16"))
  (should (equal org-notion-host "api.notion.com"))
  (should (equal org-notion-endpoint "https://api.notion.com/v1/")))

(ert-deftest org-notion-token--no-auth-source-ok ()
  (let ((org-notion-use-auth-source nil))
    (should (equal (org-notion-token "token-test") "token-test"))))

;; FIXME 2022-01-02: currently requires ~/.authinfo.gpg
(ert-deftest org-notion-token--auth-source-ok ()
  (let ((org-notion-use-auth-source t))
    (should (org-notion-token))))

(ert-deftest org-notion-current-user-ok ()
  (should (org-notion-get-current-user)))

(ert-deftest org-notion-get-users-ok ()
  (should (org-notion-get-users)))

(ert-deftest org-notion-search-ok ()
  (should (org-notion-search "org-notion")))

;; Org-mode tests
(ert-deftest org-notion-to-org-time-ok ()
  (set-time-zone-rule t)
  (should (string= (org-notion-to-org-time "2022-01-09T08:59:15.000Z") "2022-01-09 08:59:15")))

(ert-deftest org-notion-from-org-time-ok ()
  (set-time-zone-rule t)
  (should (string= (org-notion-from-org-time "2022-01-09 08:59:15") "2022-01-09T08:59:15+0000")))

(ert-deftest org-notion-id-at-point-ok ()
  (with-temp-buffer
    (insert-file-contents "mock.org")
    (let ((id "64adb50d17394203a31265641aeaeb8e")
	  (pom (point-min)))
      (should (equal (org-notion-id-at-point pom) id)))))

(provide 'org-notion-test)
;;; org-notion-test.el ends here
