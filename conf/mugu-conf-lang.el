;;; mugu-conf-lang --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  :config
  (require 'lsp-ui))

(use-package lsp-ui
  :defer
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config)

(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp)

(use-package mugu-lsp
  :straight nil
  :after lsp-mode
  :commands mugu-lsp-menu)

(provide 'mugu-conf-lang)
;;; mugu-conf-lang ends here
