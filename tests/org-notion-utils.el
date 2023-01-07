(defun insert-random-uuid ()
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

(provide 'org-notion-utils)
