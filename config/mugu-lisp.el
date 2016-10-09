(use-package eldoc
  :ensure
  :defer
  :diminish eldoc-mode
  )

(use-package slime
  :ensure slime
  :defer)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :defer t
  :commands my-jump-to-elisp-docs
  :diminish elisp-slime-nav-mode 
  :init
  (progn
    (defun my-lisp-hook ()
      (progn
        (elisp-slime-nav-mode)
        (eldoc-mode +1)
        )
      )
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
    (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
    (add-hook 'ielm-mode-hook 'my-lisp-hook)
    (defun my-jump-to-elisp-docs (sym-name)
      "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
      (interactive (list (elisp-slime-nav--read-symbol-at-point)))
      (help-xref-interned (intern sym-name))
      (switch-to-buffer-other-window "*Help*" t))
    )
  :config 
  (after 'mugu-hydra
    (defhydra mugu-lisp-help (:color teal
                                     :hint nil)
      "help lisp"
      ("f" counsel-describe-function "describe function")
      ("v" counsel-describe-variable "descibe variable"))
    (mugu-hydra-register-mode-hook 'emacs-lisp-mode-hook 'mugu-lisp-help/body))
  (after 'evil
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
      'my-jump-to-elisp-docs)))

(provide 'mugu-lisp)
