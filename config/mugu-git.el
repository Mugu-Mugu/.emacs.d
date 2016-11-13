(require 'mugu-menu)

(use-package magit
  :ensure
  :defer
  :bind (:map magit-mode-map ("SPC" . mugu-menu-main-menu)))

(use-package mugu-git-menu
  :after magit)

(provide 'mugu-git)
