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
(use-package blacken :defer :disabled "Very slow and resource hungry")
(use-package python-black :defer
  :hook (python-mode . python-black-on-save-mode))

(use-package lsp-pyright
  :defer
  :custom
  (lsp-pyright-multi-root nil)
  )


;; mugu-lsp-pyright

(provide 'mugu-conf-python)
;;; mugu-conf-python ends here
