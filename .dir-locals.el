;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((compile-command . "make all")))
 (org-mode . ((time-stamp-pattern . "4/#\\+DATE: %Y-%02m-%02d$")
	      (eval . (add-hook 'before-save-hook 'time-stamp nil t)))))
