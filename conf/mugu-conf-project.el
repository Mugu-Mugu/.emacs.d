;;; mugu-conf-project --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)
(require 'mugu-menu)

(use-package projectile
  :diminish projectile-mode
  :after mugu-project
  :defer
  :config
  (setq projectile-completion-system 'ivy)
  (add-hook 'kill-emacs-hook 'projectile-save-known-projects)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-project-search-path '("~/work/"))
  (projectile-mode 1))

(use-package mugu-project
  :straight nil
  :defer
  :commands (mugu-project-menu)
  :config
  (mugu-project-activate))

(provide 'mugu-conf-project)
;;; mugu-conf-project ends here
