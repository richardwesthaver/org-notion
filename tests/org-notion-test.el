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

(defun get-results (json)
  (when (equal (cdar json) "list")
    (let ((results (alist-get 'results json)))
      (dolist (i (append results nil)))
      results)))

(ert-deftest consts-ok ()
  (should (equal org-notion-version "2021-08-16"))
  (should (equal org-notion-host "api.notion.com"))
    (insert-file-contents "mock.org")
    (let (())
      (should (equal )))))
(provide 'org-notion-test)
;;; org-notion-test.el ends here
