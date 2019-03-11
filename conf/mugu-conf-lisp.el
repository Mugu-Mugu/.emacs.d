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
     (additional-movement normal visual motion))))

(use-package mugu-lisp
  :after evil-lispy
  :straight nil
  :config
  (mugu-menu-register-mode-menu 'emacs-lisp-mode 'mugu-lisp-main-menu))

(use-package eldoc
  :defer
  :diminish eldoc-mode)

(use-package elisp-slime-nav
  :defer)

(provide 'mugu-conf-lisp)
;;; mugu-conf-lisp ends here
