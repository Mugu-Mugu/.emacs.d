(use-package company
  :ensure
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
  :after 'company
  :config (company-flx-mode +1)
  )

(use-package company-quickhelp
  :ensure
  :after 'company
  :config (progn
            (company-quickhelp-mode 1)
            (setq company-quickhelp-delay 0.3)
            )
  )

(use-package ivy
  :ensure
  :demand
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-wrap t)
  )

(use-package counsel
  :ensure
  :after ivy
  :config
  (setq counsel-rg-find-recursive "rg --files -g *%s*")

  (defun counsel-rg-function (string extra-ag-args)
    "Grep in the current directory for STRING.
If non-nil, EXTRA-AG-ARGS string is appended to `counsel-rg-find-recursive'."
    (when (null extra-ag-args)
      (setq extra-ag-args ""))
    (if (< (length string) 3)
        (counsel-more-chars 3)
      (let ((default-directory counsel--git-grep-dir)
            (regex (concat (replace-regexp-in-string " " "*" string) )))
        (let ((ag-cmd (format counsel-rg-find-recursive
                              (concat regex ))))
          (message ag-cmd)
          (if (file-remote-p default-directory)
              (split-string (shell-command-to-string ag-cmd) "\n" t)
            (counsel--async-command ag-cmd)
            nil)))))
  )
  (defun counsel-rg-find-file-recursive (&optional initial-input initial-directory extra-ag-args ag-prompt)
    "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-rg-find-recursive'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument. "
    (interactive
     (list nil
           (when current-prefix-arg
             (read-directory-name (concat
                                   (car (split-string counsel-rg-find-recursive))
                                   " in directory: ")))))
    (ivy-set-prompt 'counsel-ag counsel-prompt-function)
    (setq counsel--git-grep-dir (or initial-directory default-directory))
    (ivy-read (or ag-prompt (car (split-string counsel-rg-find-recursive)))
              (lambda (string)
                (counsel-rg-function string extra-ag-args))
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action 
              (lambda (x)
                (message ivy--directory)
                (with-ivy-window
                  (find-file (expand-file-name x ivy--directory))))
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-rg-find-file-recursive)

  )

(use-package swiper
  :ensure
  :after ivy
  )

(use-package ivy-hydra
  :ensure
  :after ivy
  )



(provide 'mugu-completion)
