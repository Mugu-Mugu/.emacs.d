;;; mugu-window --- Basic settings for vanilla emacs window -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'hydra)
(require 'winner)
(require 'use-package)
(require 'ace-window)
(require 'mugu-window-utils)

(defmenu mugu-window-resize-menu
  (:color red :hint nil)
  "
                             -- RESIZING WINDOW --
"
  ("b" balance-windows "balance window height" :column "0-Set")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "maximize current window")
  ("h" shrink-window-horizontally "↔ shrink"  :column "1-Shrink")
  ("j" shrink-window  "↕ shrink")
  ("H" (shrink-window-horizontally 20) "↔ shrink ++")
  ("J" (shrink-window 20) "↕ shrink ++")
  ("k" "enlarge-window" "↔ enlarge" :column "2-Enlarge")
  ("l" enlarge-window-horizontally "↕ enlarge")
  ("K" (enlarge-window 20) "↔ enlarge")
  ("L" (enlarge-window-horizontally 20) "↕ enlarge")
  ("RET" mugu-window-menu "return to window menu" :color blue :column nil)
  ("q" nil "quit menu" :color blue :column nil))

(defmenu mugu-window-menu
  (:color red :hint nil :timeout 4)
  "
                               -- WINDOW MENU --

"
  ("z" ace-window "ace" :color blue :column "1-Switch")
  ("h" windmove-left "← window")
  ("j" windmove-down "↓ window")
  ("k" windmove-up "↑ window")
  ("l" windmove-right "→ window")
  ("s" split-window-below "split window" :color blue :column "2-Split Management")
  ("v" split-window-right "split window vertically" :color blue)
  ("d" delete-window "delete current window")
  ("D" mugu-window-delete-all-windows "delete *all* other windows")
  ("u" winner-undo "undo window conf" :column "3-Undo/Redo")
  ("r" winner-redo "redo window conf")
  ("b" balance-windows "balance window height" :column "4-Sizing")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "maximize current window")
  ("c" mugu-window-resize-menu "resize window size submenu" :color blue)
  ("q" nil "quit menu" :color blue :column nil))

(provide 'mugu-window)
;;; mugu-window ends here
