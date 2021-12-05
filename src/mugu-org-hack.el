;;; mugu-org-hack --- Summary
;; to override the shit default version provided by emacs.
;;; Commentary:

;;; Code:


(require 'subr-x)
(require 'use-package)

;; (use-package git)
;; (require 'git)

;; (defun org-git-version ()
;;   "The Git version of `org-mode'.
;; Inserted by installing `org-mode' or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (git-run "describe"
;;               "--match=release\*"
;;               "--abbrev=6"
;;               "HEAD"))))

;; (defun org-release ()
;;   "The release version of `org-mode'.
;; Inserted by installing `org-mode' or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (string-remove-prefix
;;       "release_"
;;       (git-run "describe"
;;                "--match=release\*"
;;                "--abbrev=0"
;;                "HEAD")))))

;; (provide 'org-version)

(defun mugu-org-hack--fix-add-log (&rest _)
  "For some reasons, org log note is implemented with postcommand hook.
This conflict with many other packages competing for it.
In any case there was no good reasons to implement it this way since a standard call is enough."
  (remove-hook 'post-command-hook #'org-add-log-note)
  (org-add-log-note))

(defun mugu-org-hack--switch-window(_orig-fun &rest args)
  "Ugly but the original implementation popped frame when side window was open.
This is because of the macro `org-no-popups' which actually did the opposite of
what it was intended to do.  Since it also unbound `display-buffer-alist' it was
not possible to fix it otherwise.
ORIG-FUN and ARGS are not read."
  (apply #'switch-to-buffer-other-window args))

(defun mugu-org-hack--activate ()
  "Install org hacks."
  (advice-add 'org-add-log-setup :after #'mugu-org-hack--fix-add-log)
  (advice-add #'org-switch-to-buffer-other-window :around #'mugu-org-hack--switch-window))

(defun mugu-org-hack--deactivate ()
  "Remove org hacks."
  (advice-remove #'org-switch-to-buffer-other-window #'mugu-org-hack--switch-window)
  (advice-remove 'org-add-log-setup #'mugu-org-hack--fix-add-log))

(define-minor-mode mugu-org-hack-mode
  "Activate various org mode hacks"
  :global t
  (if mugu-org-hack-mode
      (mugu-org-hack--activate)
    (mugu-org-hack--deactivate)))

(provide 'mugu-org-hack)
;;; mugu-org-hack ends here
