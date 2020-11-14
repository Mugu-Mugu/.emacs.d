;;; Package --- Summary
;; tbc
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package evil
  :demand
  :diminish
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-C-i-jump t)
  (evil-jumps-cross-buffers nil)
  (evil-overriding-maps nil)
  (evil-symbol-word-search t)
  :general
  (:states 'insert
           "C-d" nil)
  :hook (evil-jumps-post-jump . recenter)
  :config
  (evil-mode 1)
  (defalias #'forward-evil-word #'forward-evil-symbol))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package general
  ;; hack to unbind SPC
  :after evil-collection
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'mugu-space-main-menu))

(use-package evil-surround
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\{ . ("{" . "}"))

                               (?\) . ("(" . ")"))
                               (?\] . ("[" . "]"))
                               (?\} . ("{" . "}"))

                               (?# . ("#{" . "}"))
                               (?b . ("(" . ")"))
                               (?B . ("{" . "}"))
                               (?> . ("<" . ">"))
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)
                               (?f . evil-surround-function)))
  :general
  (:states 'operator
           "s" #'evil-surround-edit
           "s" #'evil-surround-edit)
  (:states 'visual
           "s" #'evil-surround-region)
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :custom
  (evilmi-shortcut "%")
  :general
  (:states '(normal visual)
           "%" #'evilmi-jump-items)
  :config (global-evil-matchit-mode 1))

(use-package evil-commentary
  :general
  (:states 'normal
           "gc" 'evil-commentary
           "gy" 'evil-commentary-yank)
  :delight
  :config (evil-commentary-mode))

(use-package evil-mc
  :diminish
  :defer
  :general (:states
            '(normal visual)
            "C-n" #'evil-mc-make-and-goto-next-match
            "C-p" #'evil-mc-make-and-goto-prev-match)
  :config
  (global-evil-mc-mode)
  (add-to-list 'evil-mc-incompatible-minor-modes 'lispy-mode)
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-lispy-mode))

(use-package mugu-mc
  :straight nil
  :commands mugu-mc-menu
  :general
  (:states '(normal visual)
           "," #'mugu-mc-menu))

(use-package evil-numbers
  :defer
  :general
  (:states 'normal
           "C-a" #'evil-numbers/inc-at-pt
           "C-x" #'evil-numbers/dec-at-pt))

(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-lion
  :defer
  :general
  (:states '(normal visual)
           "gl" #'evil-lion-left
           "gL" #'evil-lion-right))

(use-package evil-exchange
  :disabled "default bindings are conflicted and this package is less useful than other"
  :defer
  :general
  (:state '(normal visual)
          "cx" #'evil-exchange
          "cX" #'evil-exchange-cancel))

(use-package evil-goggles
  :disabled "It does exactly nothing!"
  :defer 1
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse t))

(use-package evil-owl
  :defer 2
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 100 :height 50)
        evil-owl-max-string-length 50)
  (evil-owl-mode)
  :custom
  (evil-owl-idle-delay 0.0))

(use-package evil-escape
  ;; I may use the super escape provided here but I do not need nor have a binding for it
  :after evil
  :config
  (evil-escape-mode))

(provide 'mugu-conf-evil)
;;; mugu-conf-evil ends here
