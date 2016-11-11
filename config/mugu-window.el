(require 'hydra)

(winner-mode +1)
(add-to-list 'display-buffer-alist
             (quote ("\\*Help\\*" . ((display-buffer-in-side-window)
                                     .
                                     ((side . right)
                                      (window-height . 1)
                                      (window-width . 80)
                                      (inhibit-switch-frame . t)
                                      (inhibit-same-window . t))))))

(defhydra mugu-window-resize-hydra
  (:color red :hint nil)
  "
                             -- RESIZING WINDOW --
"
  ("h" shrink-window-horizontally "↔ shrink"  :column "1-Shrink")
  ("j" shrink-window  "↕ shrink")
  ("H" (shrink-window-horizontally 20) "↔ shrink ++")
  ("J" (shrink-window 20) "↕ shrink ++")
  ("k" "enlarge-window" "↔ enlarge" :column "2-Enlarge")
  ("l" enlarge-window-horizontally "↕ enlarge")
  ("K" (enlarge-window 20) "↔ enlarge")
  ("L" (enlarge-window-horizontally 20) "↕ enlarge")
  ("RET" mugu-window-menu "return to window menu" :color blue :column nil)
  ("q" nil "quit menu" :color blue :column nil)
  ("SPC" mugu-menu-main-menu "return to main menu" :color blue))

(defhydra mugu-window-hydra
  (:color red :hint nil :timeout 4)
  "
                               -- WINDOW MENU --

"
  ("z" ace-window "ace" :color blue :column "1-Switch")
  ("h" windmove-left "← window")
  ("j" windmove-down "↓ window")
  ("k" windmove-up "↑ window")
  ("l" windmove-right "→ window")
  ("sp" split-window-below "split window" :color blue :column "2-Split Management")
  ("vsp" split-window-right "split window vertically" :color blue)
  ("d" delete-window "delete current window")
  ("D" delete-other-windows "delete *all* other windows")
  ("f" follow-mode "toogle follow mode")
  ("u" winner-undo "undo window conf" :column "3-Undo/Redo")
  ("r" winner-redo "redo window conf")
  ("b" balance-windows "balance window height" :column "4-Sizing")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "maximize current window")
  ("c" mugu-window-resize-hydra/body "resize window size submenu" :color blue)
  ("q" nil "quit menu" :color blue :column nil)
  ("SPC" mugu-menu-main-menu "return to main menu" :color blue))


(defalias 'mugu-window-menu  'mugu-window-hydra/body )
(mugu-menu-register-permanent-menu ("z" mugu-window-menu "windows menu" :color blue))



(provide 'mugu-window)
