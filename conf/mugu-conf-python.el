;;; mugu-conf-python --- Summary
;; tbc
;;; Commentary:

;;; Code:

;;; Code:
;; For elpy
(require 'use-package)

(use-package python-mode
  :straight nil
  :hook
  (python-mode . lsp)
  (python-mode . flycheck-mode)
  (python-mode . mugu-lang-mode)
  (python-mode . mugu-lsp-mode)
  :config
  :custom
  (python-shell-interpreter "python3"))

(use-package mugu-pyright
  :straight nil
  :hook
  (python-mode . mugu-pyright-mode))

(use-package isortify :defer :disabled "Completely bugged")
(use-package blacken :defer)
(use-package lsp-pyright :defer)

;; mugu-lsp-pyright

(provide 'mugu-conf-python)
;;; mugu-conf-python ends here
