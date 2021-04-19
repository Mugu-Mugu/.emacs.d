;;; mugu-conf-window --- Configuration for window ui -*- lexical-binding: t -*-
;;; Commentary:

(require 'use-package)

(use-package mugu-window-defaults
  :straight nil
  :demand
  :config
  (mugu-window-defaults-activate))

(use-package mugu-window
  :straight nil
  :commands (mugu-window-menu)
  :general (:states 'motion "<backspace>" #'mugu-window-delete-next-side))

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
  :pretty-hydra
  (mugu-tab-menu
   (:color blue :hint nil)
   ("tab management"
    (("n" mugu-tab-new "new" :color red)
     ("c" mugu-tab-clone "clone" :color red)
     ("d" mugu-tab-delete "delete" :color red)
     ("r" mugu-tab-rename "rename" :color red))
    "tab switch"
    (("s" mugu-tab-switch "switch")
     ("k" mugu-tab-switch-to-previous "←")
     ("j" mugu-tab-switch-to-next "→"))
    "buffer actions"
    (("p" mugu-tab-pin-buffer "pin" :color blue)
     ("u" mugu-tab-unpin-buffer "unpin" :color blue)))))

(use-package mugu-tab-project
  :straight nil
  :after mugu-project
  :config (mugu-tab-project-mode))

(use-package mugu-project-vterm
  :straight nil
  :after mugu-tab
  :config
  (mugu-tab-ensure-tab-rule-prioritary))

;;; Code:
;; -*- lexical-binding: t -*-
(provide 'mugu-conf-window)
;;; mugu-conf-window ends here
