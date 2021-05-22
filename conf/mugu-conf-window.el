;;; mugu-conf-window --- Configuration for window ui -*- lexical-binding: t -*-
;;; Commentary:

(require 'use-package)

(use-package mugu-window
  :straight nil
  :config
  (mugu-window-mode)
  :general
  (:states 'motion
           "s-<backspace>" #'mugu-window-delete-or-toogle-lru-side
           "<backspace>" #'mugu-window-delete-or-toogle-mru-side)
  :hydra
  (mugu-window-resize-menu
   (:color red :hint nil)
   "Window resize menu"
   ("b" balance-windows "balance window height" :column "0-Set")
   ("m" maximize-window "maximize current window")
   ("M" minimize-window "maximize current window")
   ("h" shrink-window-horizontally "↔ shrink" :column "1-Shrink")
   ("j" shrink-window "↕ shrink")
   ("H" (shrink-window-horizontally 20) "↔ shrink ++")
   ("J" (shrink-window 20) "↕ shrink ++")
   ("k" "enlarge-window" "↔ enlarge" :column "2-Enlarge")
   ("l" enlarge-window-horizontally "↕ enlarge")
   ("K" (enlarge-window 20) "↔ enlarge")
   ("L" (enlarge-window-horizontally 20) "↕ enlarge")
   ("RET" mugu-window-menu/body "return to window menu" :color blue :column nil)
   ("q" nil "quit menu" :color blue :column nil))
  (mugu-window-menu
   (:color red :hint nil :timeout 4)
   "Window menu"
   ("z" (progn
          (other-window 1)
          (when (<= (length (window-list)) 3)
            (setq hydra-deactivate t))) "switch" :column "1-Switch")
   ("h" windmove-left "← window")
   ("j" windmove-down "↓ window")
   ("k" windmove-up "↑ window")
   ("l" windmove-right "→ window")
   ("s" split-window-below "split window" :color blue :column "2-Split Management")
   ("v" split-window-right "split window vertically" :color blue)
   ("d" mugu-window-delete "delete current window")
   ("D" delete-other-windows "delete *all* other windows")
   ("u" winner-undo "undo window conf" :column "3-Undo/Redo")
   ("r" winner-redo "redo window conf")
   ("b" balance-windows "balance window height" :column "4-Sizing")
   ("m" maximize-window "maximize current window")
   ("M" minimize-window "maximize current window")
   ("c" mugu-window-resize-menu/body "resize window size submenu" :color blue)
   ("q" nil "quit menu" :color blue :column nil)))

(use-package zoom
  :disabled
  :defer 5
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package golden-ratio
  :disabled
  :defer 5
  :config
  (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1))

(use-package mugu-tab
  :straight nil
  :config
  (mugu-tab-mode +1)
  (setq tab-bar-history-limit 40)
  :custom
  (tab-bar-show 1)
  :pretty-hydra
  (mugu-tab-menu
   (:color red :hint nil)
   ("tab management"
    (("n" mugu-tab-new "new")
     ("c" mugu-tab-clone "clone" )
     ("d" mugu-tab-delete "delete")
     ("r" mugu-tab-rename "rename"))
    "tab switch"
    (("s" mugu-tab-switch "switch" :color blue)
     ("k" mugu-tab-switch-to-previous "←")
     ("j" mugu-tab-switch-to-next "→"))
    "buffer actions"
    (("p" mugu-tab-pin-buffer "pin" :color blue)
     ("u" mugu-tab-unpin-buffer "unpin" :color blue)))))

(use-package mugu-tab-project
  :straight nil
  :after mugu-project
  :config (mugu-tab-project-mode))

(use-package mugu-window
  :straight nil
  :after robe
  :config
  (mugu-window-configure-side-window "\\*robe-doc\\*" 'top 0.2))

;;; Code:
;; -*- lexical-binding: t -*-
(provide 'mugu-conf-window)
;;; mugu-conf-window ends here
