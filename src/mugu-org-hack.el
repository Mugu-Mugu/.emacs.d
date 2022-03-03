;;; mugu-org-hack --- Summary
;; to override the shit default version provided by emacs.
;;; Commentary:

;;; Code:


(require 'subr-x)
(require 'use-package)

(defun mugu-org-hack--fix-add-log (&rest _)
  "For some reasons, org log note is implemented with postcommand hook.
This conflict with many other packages competing for it.
In any case there was no good reasons to implement it this way since a standard call is enough."
  (remove-hook 'post-command-hook #'org-add-log-note)
  (org-add-log-note))

(defun mugu-org-hack--activate ()
  "Install org hacks."
  (advice-add 'org-add-log-setup :after #'mugu-org-hack--fix-add-log))

(defun mugu-org-hack--deactivate ()
  "Remove org hacks."
  (advice-remove 'org-add-log-setup #'mugu-org-hack--fix-add-log))

(define-minor-mode mugu-org-hack-mode
  "Activate various org mode hacks"
  :global t
  (if mugu-org-hack-mode
      (mugu-org-hack--activate)
    (mugu-org-hack--deactivate)))

(provide 'mugu-org-hack)
;;; mugu-org-hack ends here
