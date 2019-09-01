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
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil))

(use-package lsp-ui
  :defer
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp
  :custom (company-lsp-cache-candidates 'auto))

(use-package mugu-lsp
  :straight nil
  :after lsp-mode
  :commands mugu-lsp-menu
  :config
  (mugu-lsp-activate-ui-keymap))

(use-package dumb-jump
  :defer
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-confirm-jump-to-modified-file nil)
  :config
  (add-hook 'dumb-jump-after-jump-hook #'recenter))


(provide 'mugu-conf-lang)
;;; mugu-conf-lang ends here
