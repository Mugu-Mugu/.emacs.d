(use-package saveplace
  :straight t
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory "var/places"))
    (setq-default save-place t)))

(use-package savehist
  :straight t
  :config
    (progn
        (setq savehist-file (concat user-emacs-directory "var/savehist")
            savehist-additional-variables '(search ring regexp-search-ring)
            savehist-autosave-interval 60)
        (savehist-mode t)))

(use-package recentf
  :straight t
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory "var/recentf")
          recentf-max-saved-items 1000
          recentf-max-menu-items 500)
    (recentf-mode +1)))

(provide 'mugu-sessions-persistance)
