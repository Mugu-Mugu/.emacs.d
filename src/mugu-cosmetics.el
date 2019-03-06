;;; mugu-cosmetics --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

;; * begin:
(use-package all-the-icons :defer)
(use-package font-lock+ :defer)
(use-package beacon
  :defer 3
  :delight
  :config
  (setq beacon-color "darkorange")
  (beacon-mode))
(use-package minimap
  :defer 3
  :delight
  :config (minimap-mode))

(use-package mugu-cosmetics-utils
  :straight nil
  :config
  (mugu-cosmetics-activate))

(provide 'mugu-cosmetics)
;;; mugu-cosmetics ends here
