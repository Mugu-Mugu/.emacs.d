;;; mugu-conf-lint --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'use-package)

(use-package flycheck
  :diminish flycheck-mode
  :defer
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.5)
  (flycheck-idle-buffer-switch-delay 2.0)
  (flycheck-idle-change-delay 2.0)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package mugu-flycheck
  :straight nil
  :after flycheck
  :commands mugu-flycheck-menu
  :config
  (mugu-flycheck-set-window-conf))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(provide 'mugu-conf-lint)
;;; mugu-conf-lint ends here
