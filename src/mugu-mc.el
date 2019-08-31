;;; mugu-mc --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'evil-mc)
(require 'mugu-menu)


(defun mugu-mc--finalize-menu ()
  "Cleanup after mc."
  (hydra-keyboard-quit)
  (remove-hook 'evil-insert-state-entry-hook #'mugu-mc--finalize-menu))

(defun mugu-mc--prepare-menu ()
  "Install auto leave hook for multi cursor menu."
  (add-hook 'evil-insert-state-entry-hook #'mugu-mc--finalize-menu))

(defmenu mugu-mc-menu (:color pink :hint nil :pre (mugu-mc--prepare-menu))
  ("rm" evil-mc-make-all-cursors "make all cursor" :column "global actions")
  ("ru" evil-mc-undo-all-cursors "undo-all-cursors")
  ("rs" evil-mc-pause-cursors "pause-cursors" :color blue)
  ("rr" evil-mc-resume-cursors "mc-resume-cursors")
  ("rf" evil-mc-make-and-goto-first-cursor "mc-make-and-goto-first-cursor" :column "with match")
  ("rl" evil-mc-make-and-goto-last-cursor "mc-make-and-goto-last-cursor")
  ("M-n" evil-mc-make-and-goto-next-cursor "mc-make-and-goto-next-cursor")
  ("M-p" evil-mc-make-and-goto-prev-cursor "mc-make-and-goto-prev-cursor")
  ("C-n" evil-mc-make-and-goto-next-match "mc-make-and-goto-next-match")
  ("C-p" evil-mc-make-and-goto-prev-match "mc-make-and-goto-prev-match")
  ("rN" evil-mc-skip-and-goto-next-cursor "mc-skip-and-goto-next-cursor" :column "skip match")
  ("rP" evil-mc-skip-and-goto-prev-cursor "mc-skip-and-goto-prev-cursor")
  ("rn" evil-mc-skip-and-goto-next-match "mc-skip-and-goto-next-match")
  ("rp" evil-mc-skip-and-goto-prev-match "mc-skip-and-goto-prev-match")
  ("rh" evil-mc-make-cursor-here "mc-make-cursor-here" :column "without match")
  ("rk" evil-mc-make-cursor-move-prev-line "mc-make-cursor-move-prev-line")
  ("rj" evil-mc-make-cursor-move-next-line "mc-make-cursor-move-next-line")
  ("q" nil "exit" :column nil)
  ("," nil "exit"))

(provide 'mugu-mc)
;;; mugu-mc ends here
