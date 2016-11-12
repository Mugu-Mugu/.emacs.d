(use-package ivy
  :ensure
  :commands ivy-mode
  :diminish ivy-mode
  :defer 
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
  (setq ivy-wrap t))

(use-package  smex
  :ensure
  :defer)

(use-package ivy-hydra
  :ensure
  :after ivy
  :bind 
  (:map ivy-minibuffer-map
        ("C-o"   . soo-ivy/body))
  :config
  (defhydra soo-ivy (:hint nil :color amaranth)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^ ^ _h_ ^+^ _l_ ^ ^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("j" ivy-next-line)
    ("k" ivy-previous-line)
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("C-o" nil)
    ("i" nil)
    ("TAB" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ;; ("d" ivy-done :exit t)
    ("RET" ivy-done :exit t)
    ("C-m" ivy-done :exit t)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("o" ivy-occur :exit t))
  (after 'key-chord
    (key-chord-define ivy-minibuffer-map "jk" 'soo-ivy/body)
    )
  )


(use-package counsel
  :ensure
  :after ivy
  :diminish counsel-mode
  :config
  (counsel-mode +1)
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
  )

(use-package swiper
  :ensure
  :defer
  :after ivy
  )

(use-package helm
  :ensure 
  :defer 
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-split-window-default-side (quote other))
    (setq helm-split-window-in-side-p nil)

    (define-key helm-map (kbd "C-p") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-n") 'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (after 'helm-files
      (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
      (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
      (define-key helm-find-files-map (kbd "C-r")           'helm-find-files-down-last-level)
      )

    (defun helm-jump ()
      "Find files with helm, but be smart about buffers and recent files."
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil))
        (helm-other-buffer '(helm-projectile-sources-list
                             helm-source-buffers-list
                             helm-source-recentf
                             helm-source-bookmarks
                             helm-source-file-cache
                             helm-source-files-in-current-dir
                             helm-source-locate
                             helm-source-buffer-not-found)
                           "*helm jump*")))

    (setq helm-command-prefix-key "C-c h")
    (setq helm-quick-update t)
    )
  )


(provide 'mugu-interactive)
