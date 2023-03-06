;;; org-notion-test.el --- utils for org-notion -*- lexical-binding: t; -*-

(defun insert-random-uuid ()
  "very inefficient, but easy to understand."
  (interactive)
  (insert
   (format
    "%04x%04x-%04x-%04x-%04x-%06x%06x"
    (random (expt 16 4))
    (random (expt 16 4))
    (random (expt 16 4))
    (random (expt 16 4))
    (random (expt 16 4))
    (random (expt 16 6))
    (random (expt 16 6)))))

(defun unload-org-notion ()
  "unload org-notion."
  (interactive)
  (mapc #'unload-feature '(org-notion-test org-notion)))

(defun load-org-notion ()
  "load org-notion."
  (interactive)
  (mapc #'load-file '("../org-notion.el" "org-notion-test.el")))

(provide 'org-notion-utils)
