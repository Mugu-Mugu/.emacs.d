;;; Package --- Summary
;; TBC
;;; Commentary:

;;; Code:

(require 'evil)
(require 'general)
(require 'use-package)

(use-package lispy
  :defer
  :hook (emacs-lisp-mode . lispy-mode)
  :delight)

(use-package evil-lispy
  :delight
  :hook (emacs-lisp-mode . evil-lispy-mode)
  :config
  (key-chord-define evil-lispy-state-map "jk" 'evil-normal-state)
  (setq-default lispy-outline-header ";; ")
  (general-define-key :states 'normal :keymaps 'emacs-lisp-mode-map
                      "<M-return>" (lambda ()
                                     (interactive)
                                     (call-interactively #'lispy-meta-return)
                                     (end-of-line)
                                     (evil-insert-state)))
  (evil-define-key 'motion evil-lispy-mode-map
    (kbd "C-&") #'lispy-describe-inline
    (kbd "C-é") #'lispy-arglist-inline)
  (evil-define-key 'insert evil-lispy-mode-map
    (kbd "C-&") #'lispy-describe-inline
    (kbd "C-é") #'lispy-arglist-inline))

  ;; used to make evil normal mode commands safe with regards to lisp paren balancing
(use-package lispyville
  :delight
  :hook (emacs-lisp-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     (escape insert)
     pretiffy
     (normal visual motion))))

(use-package lispyville
  :after (lispy evil-lispy)
  :general
  (:keymaps '(evil-lispy-mode-map lispyville-mode-map lispy-mode-map) :states 'insert
            "M-k" nil
            "[" nil
            "]" nil))

(use-package mugu-lisp
  :straight nil
  :hook (emacs-lisp-mode . mugu-elisp-minor-mode)
  :general
  (:keymaps 'mugu-elisp-minor-mode-map
            [remap mugu-lang-additional-menu] #'mugu-lisp-additional-menu
            [remap mugu-lang-format-buffer] #'mugu-lisp-prettify
            [remap mugu-lang-find-definition] #'elisp-slime-nav-find-elisp-thing-at-point
            [remap mugu-menu-call-mode-menu] #'mugu-lisp-lang-menu))

(use-package checkdoc
  :straight nil
  :defer
  :custom
  (checkdoc-force-docstrings-flag nil))

(use-package eldoc
  :defer
  :custom
  (eldoc-idle-delay 0.1)
  :diminish eldoc-mode)

(use-package elisp-slime-nav
  :defer)

(provide 'mugu-conf-lisp)
;;; mugu-conf-lisp ends here
