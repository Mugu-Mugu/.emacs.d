;;; mugu-conf-cosmetics --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

;; * begin:
(use-package all-the-icons :defer :disabled)
(use-package font-lock+ :defer :disabled)
(use-package beacon
  ;; :defer 3
  :delight
  :custom
  (beacon-color "darkorange")
  :config
  (beacon-mode)
  :disabled "consume a little too much"
  )

(use-package minimap
  :defer 3
  :disabled
  :delight
  :config
  (minimap-mode))

(use-package display-line-numbers
  ;; :defer 3
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type t)
  :config
  (global-display-line-numbers-mode))

(use-package dimmer
  ;; :defer 3
  :custom
  (dimmer-fraction 0.15)
  :config
  (dimmer-mode))

(use-package mugu-cosmetics
  :straight nil
  :config
  (mugu-cosmetics-activate))

(use-package selectric-mode
  ;; very nice and fun but bugged :(
  :disabled
  :delight selectric-mode
  :config
  (selectric-mode))

(use-package smooth-scrolling
  :defer 4
  :disabled
  :config
  (smooth-scrolling-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config)

(provide 'mugu-conf-cosmetics)
;;; mugu-conf-cosmetics ends here
