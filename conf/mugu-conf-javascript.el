;;; mugu-conf-javascript --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'mugu-menu)

;; * begin:
(use-package js-mode
  :straight nil
  :defer
  :custom
  (js-jsx-indent-level 2)
  (js-indent-level 2))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :delight
  :custom
  (prettier-js-show-errors 'echo))

(use-package mugu-js
  :straight nil
  :hook
  (js-mode . lsp)
  (js-mode . mugu-js-configure-file)
  :config
  (mugu-js-configure-package)
  )

(use-package js2-mode
  :disabled "Because js-mode is better on 27 and is officialy recommanded by js2 team"
  :defer
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package rjsx-mode
  :disabled "Because js-mode is better on 27"
  :defer
  :init
  (mugu-menu-register-mode-menu 'rjsx-mode 'mugu-lsp-menu)
  :mode ("\\.js\\'" . rjsx-mode)
  :hook (js2-mode . lsp)
  :config
  (require 'lsp-mode))

(provide 'mugu-conf-javascript)
;;; mugu-conf-javascript ends here
