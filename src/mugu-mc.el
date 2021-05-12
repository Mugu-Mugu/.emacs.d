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
  ("n" evil-mc-make-and-goto-next-cursor "mc-make-and-goto-next-cursor" :color red)
  ("p" evil-mc-make-and-goto-prev-cursor "mc-make-and-goto-prev-cursor" :color red)
  ("C-n" evil-mc-make-and-goto-next-match "mc-make-and-goto-next-match" :color red)
  ("C-p" evil-mc-make-and-goto-prev-match "mc-make-and-goto-prev-match" :color red)
  ("vb" evil-mc-make-cursor-in-visual-selection-beg "evil-mc-make-cursor-in-visual-selection-bug" :column "in visual region")
  ("ve" evil-mc-make-cursor-in-visual-selection-end "evil-mc-make-cursor-in-visual-selection-bug")
  ("N" evil-mc-skip-and-goto-next-match "mc-skip-and-goto-next-match" :column "skip match")
  ("P" evil-mc-skip-and-goto-prev-match "mc-skip-and-goto-prev-match")
  ("h" evil-mc-make-cursor-here "mc-make-cursor-here" :column "without match")
  ("k" evil-mc-make-cursor-move-prev-line "mc-make-cursor-move-prev-line" :color red)
  ("j" evil-mc-make-cursor-move-next-line "mc-make-cursor-move-next-line" :color red)
  ("q" nil "exit" :column nil)
  ("," nil "exit"))

(provide 'mugu-mc)
;;; mugu-mc ends here
