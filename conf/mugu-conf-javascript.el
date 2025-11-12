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
  :hook
  (js-mode . lsp)
  (js-mode . flycheck-mode)
  (js-mode . mugu-lang-mode)
  (js-mode . mugu-lsp-mode)
  :custom
  (js-jsx-indent-level 2)
  (js-indent-level 2))

(use-package prettier-js
  :hook
  (js-mode . prettier-js-mode)
  :delight
  :custom
  (prettier-js-show-errors 'echo))

(use-package js2-mode
  :disabled "Because js-mode is better on 27 and is officialy recommanded by js2 team"
  :defer
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package typescript-mode
  :defer
  :hook
  (typescript-mode . lsp)
  (typescript-mode . prettier-js-mode)
  (typescript-mode . flycheck-mode)
  (typescript-mode . mugu-lang-mode)
  (typescript-mode . mugu-lsp-mode)
   :general
   (:keymaps '(typescript-mode-map)
             [remap mugu-lang-format-buffer] #'prettier-js))

(use-package rjsx-mode
  :disabled "Because js-mode is better on 27"
  :defer
  :init
  (mugu-menu-register-mode-menu 'rjsx-mode 'mugu-lsp-menu)
  :mode ("\\.js\\'" . rjsx-mode)
  :hook (js2-mode . lsp)
  :config
  (require 'lsp-mode))

(use-package jest
  :disabled "need some refinements because it displays irrelevant information at the moment"
  :after js-mode
  :general
  (:keymaps '(js-mode-map)
            [remap mugu-lang-test-file] #'jest-file
            [remap mugu-lang-rerun-last] #'jest-last-failed
            [remap mugu-lang-test-method] #'jest-function
            [remap mugu-lang-test-signle-at-point] #'jest-file-dwim
            [remap mugu-lang-test-all-in-project] #'jest-popup
            [remap mugu-lang-test-directory] #'jest-popup))

;; (use-package dap-mode
;;   :after js-mode
;;   :config
;;   (require 'dap-firefox)
;;   )

(provide 'mugu-conf-javascript)
;;; mugu-conf-javascript ends here
