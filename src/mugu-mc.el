;;; mugu-mc --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'evil-mc)
(require 'mugu-menu)

(defmenu mugu-mc-menu (:color blue :hint nil)
  ("m" evil-mc-make-all-cursors "make all cursor" :column "global actions")
  ("u" evil-mc-undo-all-cursors "undo-all-cursors")
  ("s" evil-mc-pause-cursors "pause-cursors" :column "pause")
  ("r" evil-mc-resume-cursors "mc-resume-cursors")
  ("m" evil-mc-make-and-goto-first-cursor "mc-make-and-goto-first-cursor" :column "with match")
  ("n" evil-mc-make-and-goto-next-cursor "mc-make-and-goto-next-cursor")
  ("p" evil-mc-make-and-goto-prev-cursor "mc-make-and-goto-prev-cursor")
  ("C-n" evil-mc-make-and-goto-next-match "mc-make-and-goto-next-match")
  ("C-p" evil-mc-make-and-goto-prev-match "mc-make-and-goto-prev-match")
  ("vb" evil-mc-make-cursor-in-visual-selection-beg "evil-mc-make-cursor-in-visual-selection-bug" :column "in visual region")
  ("ve" evil-mc-make-cursor-in-visual-selection-end "evil-mc-make-cursor-in-visual-selection-bug")
  ("N" evil-mc-skip-and-goto-next-cursor "mc-skip-and-goto-next-cursor" :column "skip match")
  ("P" evil-mc-skip-and-goto-prev-cursor "mc-skip-and-goto-prev-cursor")
  ("h" evil-mc-make-cursor-here "mc-make-cursor-here" :column "without match")
  ("k" evil-mc-make-cursor-move-prev-line "mc-make-cursor-move-prev-line" :color red)
  ("j" evil-mc-make-cursor-move-next-line "mc-make-cursor-move-next-line")
  ("q" nil "exit" :column nil)
  ("," nil "exit"))

(provide 'mugu-mc)
;;; mugu-mc ends here
