;;; mugu-git-tm --- Summary
;; A wrapper for git timemachine mode
;;; Commentary:

;;; Code:
(require 'git-timemachine)
(require 'mugu-menu)
(require 'evil)

(defmenu mugu-git-tm-menu (:hint nil :color pink)
  ("M-k" git-timemachine-show-previous-revision "git-timemachine-show-previous-revision" :column "Navigate revision")
  ("M-j" git-timemachine-show-next-revision "git-timemachine-show-next-revision")
  ("M-g" git-timemachine-show-nth-revision "git-timemachine-show-nth-revision")
  ("M-t" git-timemachine-show-revision-fuzzy "git-timemachine-show-revision-fuzzy")
  ("w" git-timemachine-kill-abbreviated-revision "git-timemachine-kill-abbreviated-revision" :column "Commit information")
  ("W" git-timemachine-kill-revision "git-timemachine-kill-revision")
  ("M-b" git-timemachine-blame "git-timemachine-blame")
  ("c" (mugu-git--tm-show-commit) "git-timemachine-show-commit")
  ("q" git-timemachine-quit "git-timemachine-quit" :color blue :column nil))

(defun mugu-git-tm--active? ()
  "Predicate indicating if the timemachine is powered."
  git-timemachine-file)

(defun mugu-git-tm--quit ()
  "Quit menu and timemachine if it was active."
  (when (mugu-git-tm--active?) (git-timemachine-quit)))

(defun mugu-git--tm-show-commit ()
  "Show commit loading magit if nessecary."
  (require 'magit)
  (git-timemachine-show-commit))

(defun mugu-git-tm-configure ()
  "Configuration for timesmachine."
  (evil-set-initial-state 'git-timemachine-mode 'motion))

(defun mugu-git-tm-menu-or-activate ()
  "Activate timemachine."
  (interactive)
  (if (mugu-git-tm--active?)
      (mugu-git-tm-menu)
    (git-timemachine)
    (mugu-git-tm-menu)))

(provide 'mugu-git-tm)
;;; mugu-git-tm ends here
