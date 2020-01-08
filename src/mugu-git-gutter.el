;;; mugu-git-gutter --- Summary
;; A wrapper for git gutter mode
;;; Commentary:

;;; Code:
(require 'git-gutter)
(require 'mugu-menu)

(defmenu mugu-git-gutter-menu (:hint nil :color red)
  ("j" git-gutter:next-hunk "next hunk" :column "Navigations")
  ("k" git-gutter:previous-hunk "previous hunk")
  ("s" git-gutter:stage-hunk "stage" :column "Actions on hunk")
  ("u" git-gutter:revert-hunk "unstage")
  ("U" git-gutter:set-start-revision "discard")
  ("v" magit-status "magit" :color blue :column nil)
  ("q" nil "quit" :color blue))

(provide 'mugu-git-gutter)
;;; mugu-git-gutter ends here
