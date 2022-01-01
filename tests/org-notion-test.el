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
(require 'org-notion)
(require 'ert)

(ert-deftest org-notion-version-ok ()
  (should (equal org-notion-version "2021-08-16")))

(ert-deftest org-notion-no-auth-source-ok ()
  (let ((org-notion-use-auth-source nil))
    (org-notion-find-token "token-test")
    (should (equal org-notion-token "token-test"))
    (setq org-notion-token nil)))

;; currently requires ~/.authinfo.gpg
;; RESEARCH 2021-12-31: how to test with a temporary auth-source
(ert-deftest org-notion-auth-source-ok ()
  (let ((org-notion-use-auth-source t))
    (org-notion-find-token)
    (should org-notion-token)
    (setq org-notion-token nil)))

(ert-deftest org-notion-current-user-ok ()
  (should (org-notion-get-current-user)))

(ert-deftest org-notion-get-users-ok ()
  (should (org-notion-get-users)))

(ert-deftest org-notion-search-ok ()
  (should (org-notion-search "org-notion")))

(provide 'org-notion-test)
;;; org-notion-test.el ends here
