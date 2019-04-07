;;; mugu-conf-javascript --- Summary
;; tbc
;;; Commentary:

;;; Code:

;;; Code:

(require 'use-package)
(require 'mugu-menu)

;; * begin:
(use-package rjsx-mode
  :defer
  :init
  (mugu-menu-register-mode-menu 'rjsx-mode 'mugu-lsp-menu)
  :mode ("\\.js\\'" . rjsx-mode)
  :hook (js2-mode . lsp)
  :config
  (require 'lsp-mode))

(use-package js2-mode
  :defer
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(provide 'mugu-conf-javascript)
;;; mugu-conf-javascript ends here