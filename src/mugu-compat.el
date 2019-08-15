;;; mugu-compat --- Summary
;; tbc
;;; Commentary:
(require 's)

;;; Code:
(defun mugu-compat-wsl-p ()
  "Return non nil if Emacs run on WSL."
  (s-contains? "Microsoft" (shell-command-to-string "uname -r")))

(provide 'mugu-compat)
;;; mugu-compat ends here
