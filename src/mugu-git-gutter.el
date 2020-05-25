;;; mugu-git-gutter --- Summary
;; A wrapper for git gutter mode
;;; Commentary:

;;; Code:
(require 'git-gutter)
(require 'mugu-menu)

(defun mugu-git-gutter-close-popup ()
  "Close window with the gitgutter popup if present."
  (interactive)
  (when-let (popup-window (get-buffer-window git-gutter:popup-buffer))
    (quit-window 'kill popup-window)))

(defmacro mugu-git-gutter-action-and-close (gutter-action)
  "Execute GUTTER-ACTION and close popup."
  `(progn
     (call-interactively #',gutter-action)
     (mugu-git-gutter-close-popup)))

(defmacro mugu-git-gutter-move-with-popup (gutter-action)
  "Execute GUTTER-ACTION and display popup."
  `(progn
     (call-interactively #',gutter-action)
     (recenter)
     (git-gutter:popup-hunk)))

(defmenu mugu-git-gutter-menu (:hint nil :color red
                                     :body-pre (with-demoted-errors (git-gutter:popup-hunk))
                                     :before-exit (mugu-git-gutter-close-popup))
  ("j" (mugu-git-gutter-move-with-popup git-gutter:next-hunk) "next hunk" :column "Navigations")
  ("k" (mugu-git-gutter-move-with-popup git-gutter:previous-hunk) "previous hunk")
  ("s" (mugu-git-gutter-action-and-close git-gutter:stage-hunk) "stage" :column "Actions on hunk")
  ("u" (mugu-git-gutter-action-and-close git-gutter:revert-hunk) "unstage")
  ("d" (mugu-git-gutter-action-and-close git-gutter:revert-hunk) "discard")
  ("U" (mugu-git-gutter-action-and-close git-gutter:set-start-revision) "discard")
  ("v" magit-status "magit" :color blue :column nil)
  ("q" nil "quit" :color blue))

(provide 'mugu-git-gutter)
;;; mugu-git-gutter ends here
