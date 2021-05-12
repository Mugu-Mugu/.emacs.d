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
  (lsp-enable-snippet t)
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t))

(use-package lsp-ui
  :defer
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package mugu-lsp
  :straight nil
  :after lsp-mode
  :commands mugu-lsp-menu
  :config
  (mugu-lsp-activate-ui-keymap))

(use-package tree-sitter
  :defer
  :hook
  (python-mode . tree-sitter-hl-mode)
  (prog-mode . global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package lsp-treemacs
  :after lsp-mode)

(use-package dumb-jump
  :defer
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-confirm-jump-to-modified-file nil)
  :config
  (add-hook 'dumb-jump-after-jump-hook #'recenter))


(provide 'mugu-conf-lang)
;;; mugu-conf-lang ends here
