(require 'mugu-menu)

(use-package magit
  :ensure
  :defer
  :config
  (define-key magit-mode-map (kbd "SPC") 'mugu-menu-main-menu))

(use-package mugu-git-menu
  :after magit)

(provide 'mugu-git)
