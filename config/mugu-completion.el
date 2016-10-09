(use-package company
  :ensure
  :diminish company-mode
  :demand
  :bind
  (:map company-active-map
        ("M-j"   . company-select-next)
        ("M-k"   . company-select-previous)
        ("M-j"   . company-select-next)
        ("M-k"   . company-select-previous)
        ("<tab>" . company-complete-selection)
        ("SPC"   . mugu-company-space-exit))
  :config 
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (defun mugu-company-space-exit ()
    (interactive)
    (progn (company-abort)
           (insert " "))
    )
  )

(use-package company-flx
  :ensure
  :diminish
  :after 'company
  :config (company-flx-mode +1)
  )

(use-package company-quickhelp
  :ensure
  :diminish
  :after 'company
  :config (progn
            (company-quickhelp-mode 1)
            (setq company-quickhelp-delay 0.3)
            )
  )



(provide 'mugu-completion)
