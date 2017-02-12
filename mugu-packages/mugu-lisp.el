;; somehow emacs-lisp-mode-hook is called before loading is finished. Therefore,
;; as is, this hook is not usable with regards to lazy loading
;; Thus it must be bound only after emacs really started
(add-hook 'emacs-startup-hook #'mugu/lisp-init)
(defun mugu/lisp-init ()
  "gather all configuration for lisp mode"

  ;; mandatory package for serious lisp editing
  (use-package lispy
    :ensure
    :defer
    :diminish lispy-mode
    :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode +1))))
  
  ;; collection of some nice bindings and rebinding that evilify lispy
  (use-package evil-lispy
    :ensure
    :diminish evil-lispy-mode lispy-other-mode
    :after 'lispy
    :config
    (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
    (add-hook 'clojure-mode-hook #'evil-lispy-mode)
    (evil-lispy-mode +1)
    (key-chord-define evil-lispy-state-map "jk" 'evil-normal-state)
    (define-key lispy-mode-map (kbd "C-&") #'lispy-describe-inline)
    (define-key lispy-mode-map (kbd "C-é") #'lispy-arglist-inline)
    (evil-define-key 'insert evil-lispy-mode-map
      (kbd "C-&") #'lispy-describe-inline
      (kbd "C-é") #'lispy-arglist-inline))

  ;; used to make evil normal mode commands safe with regards to lisp paren balancing
  (use-package lispyville
    :ensure
    :after 'lispy
    :diminish lispyville-mode
    :init (add-hook 'emacs-lisp-mode-hook #'lispyville-mode))
  
  (use-package eldoc
    :ensure
    :defer
    :diminish eldoc-mode)

  (use-package slime
    :ensure
    :defer)

  (use-package elisp-slime-nav
    :ensure
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
      (switch-to-buffer-other-window "*Help*" t))
    :config
    (after 'evil
      (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
        'my-jump-to-elisp-docs))))

(provide 'mugu-lisp)