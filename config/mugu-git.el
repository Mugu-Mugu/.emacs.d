(require 'mugu-menu)

(use-package magit
  :ensure
  :defer
  :bind (:map magit-mode-map ("SPC" . mugu-menu-main-menu)))

(use-package mugu-git-menu
  :after magit
  :config
  (mugu-menu-register-mode-menu 'magit-status-mode 'mugu-magit-main-hydra/body)
  (add-hook 'magit-status-mode-hook 'mugu-magit-main-hydra/body))


(provide 'mugu-git)
