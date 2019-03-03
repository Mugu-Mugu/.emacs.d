;;; mugu-js --- Summary
;; tbc
;;; Commentary:

;;; Code:

(require 'use-package)

;; * begin:
(use-package rjsx-mode
  :defer
  :init
  (mugu-menu-register-mode-menu 'rjsx-mode 'mugu-lsp-menu)
  :mode ("\\.js\\'" . rjsx-mode)
  :hook (js2-mode . lsp)
  )

(use-package js2-mode
  :defer
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(provide 'mugu-js)
;;; mugu-js ends here
