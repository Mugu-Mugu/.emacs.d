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
  :config
  :custom
  (python-shell-interpreter "python3"))

(use-package mugu-python
  :straight nil
  :hook
  (python-mode . mugu-python-configure-file)
  :config
  (mugu-python-configure-package))

(use-package elpy
  :disabled "To consider eventually..."
  :defer t
  :config
  (elpy-enable))

(provide 'mugu-conf-python)
;;; mugu-conf-python ends here
