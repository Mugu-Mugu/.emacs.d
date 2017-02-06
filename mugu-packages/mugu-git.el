(require 'mugu-menu)

(use-package magit
  :ensure
  :defer
  :bind (:map magit-mode-map ("SPC" . mugu-menu-main-menu)))

(use-package mugu-git-menu
  :after magit
  :config
  (defun mugu-magit-register-menu-mode (the-magit-mode the-menu)
    "register the menu for the given magit mode.
That is: bind SPC SPC for the mode and autoload the menu on first buffer entering"
    (mugu-menu-register-mode-menu `,the-magit-mode `,the-menu)
    (let ((the-hook (intern-soft (concat (symbol-name the-magit-mode) "-hook"))))
      (add-hook `,the-hook `,the-menu)))
  (mugu-magit-register-menu-mode 'magit-revision-mode 'mugu-magit-default-menu/body)
  (mugu-magit-register-menu-mode 'magit-log-mode 'mugu-magit-default-menu/body)
  (mugu-magit-register-menu-mode 'magit-status-mode 'mugu-magit-status-menu/body))


(provide 'mugu-git)
