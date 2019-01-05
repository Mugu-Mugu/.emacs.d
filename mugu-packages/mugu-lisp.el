;;; Package --- Summary
;; TBC
;; * Commentary:

;;; Code:

;; somehow emacs-lisp-mode-hook is called before loading is finished. Therefore,
;; as is, this hook is not usable with regards to lazy loading
;; Thus it must be bound only after emacs really started
(require 'evil)
(require 'general)
(require 'use-package)
(add-hook 'emacs-startup-hook #'mugu/lisp-init)

;; * fe
(defun mugu/lisp-init ()

  "Gather all configuration for Lisp mode."
  ;; mandatory package for serious lisp editing
  (use-package lispy
    :defer
    :diminish lispy-mode
    :init (add-hook 'emacs-lisp-mode-hook
                    (lambda () (lispy-mode +1)))
    :config
    (message "lispy loaded"))

  ;; collection of some nice bindings and rebinding that evilify lispy
  (use-package evil-lispy
    :diminish evil-lispy-mode lispy-other-mode
    :after lispy
    :config
    (require 'lispy)
    (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
    (add-hook 'clojure-mode-hook #'evil-lispy-mode)
    (evil-lispy-mode +1)
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
    :after lispy
    :diminish lispyville-mode
    :init (add-hook 'emacs-lisp-mode-hook #'lispyville-mode))

  (use-package mugu-lisp-utils
    :defer
    :straight (:local-repo)
    :functions mugu-menu-register-mode-menu
    :commands mugu-lisp-main-menu
    :init
    (require 'mugu-menu)
    (mugu-menu-register-mode-menu 'emacs-lisp-mode 'mugu-lisp-main-menu)
    :config)

  (use-package eldoc
    :defer
    :diminish eldoc-mode)

  (use-package slime
    :defer)

  (use-package elisp-slime-nav
    :commands my-jump-to-elisp-docs
    :diminish elisp-slime-nav-mode
    :init
    (defun my-lisp-hook ()
      (progn
        (elisp-slime-nav-mode)
        (eldoc-mode 1)))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
    (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
    (add-hook 'ielm-mode-hook 'my-lisp-hook)
    (defun my-jump-to-elisp-docs (sym-name)
      "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
      (interactive (list (elisp-slime-nav--read-symbol-at-point)))
      (help-xref-interned (intern sym-name))
      (switch-to-buffer-other-window "*Help*" t))))

(provide 'mugu-lisp)
;;; mugu-lisp ends here
