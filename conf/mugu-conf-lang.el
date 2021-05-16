;;; mugu-conf-lang --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package mugu-lang
  :straight nil
  :hook
  (prog-mode . mugu-lang-mode)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :general
  (:keymaps 'mugu-lang-mode-map
            [remap mugu-lang-find-definition] #'dumb-jump-go
            [remap mugu-lang-find-reference] #'xref-find-definitions))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-prefer-flymake nil)
  (lsp-auto-guess-root t)
  (lsp-enable-snippet t)
  (lsp-completion-provider :none))

(use-package lsp-ui
  :defer
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-delay 0.1)
  (lsp-ui-sideline-enable t)
  (lsp-signature-function #'lsp-signature-posframe)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package lsp-ui
  :defer
  :general
  (:keymaps
   '(lsp-ui-doc-frame-mode-map lsp-ui-doc-mode-map)
   :states '(normal motion)
   "<space>" #'mugu-lsp-doc-hide
   "q" #'mugu-lsp-doc-hide)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-delay 5.0)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-width 400)
  (lsp-ui-doc-max-height 40))

(use-package mugu-lsp
  :straight nil
  :after lsp-mode
  :commands
  mugu-lsp-menu
  mugu-lsp-activate-for-keymap
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
  :commands dumb-jump-xref-activate
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-confirm-jump-to-modified-file nil)
  :config
  (add-hook 'dumb-jump-after-jump-hook #'recenter))


(provide 'mugu-conf-lang)
;;; mugu-conf-lang ends here
