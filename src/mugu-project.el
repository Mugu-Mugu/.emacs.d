;;; mugu-project --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)
(require 'mugu-menu)

(use-package projectile
  :diminish projectile-mode
  :after mugu-project-utils
  :defer
  :config
  (setq projectile-completion-system 'ivy)
  (add-hook 'kill-emacs-hook 'projectile-save-known-projects)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-project-search-path '("~/work/"))
  (setq projectile-switch-project-action (lambda ()
                                           (message "%s default directory (buffer name %s)" default-directory (buffer-name (current-buffer)))
                                           (cd default-directory)
                                           (message "%s root dir" (projectile-project-root))))
  (projectile-mode 1))

(use-package persp-projectile
  :after mugu-project-utils
  :defer)

(use-package perspective
  :defer
  :after mugu-project-utils
  :config
  (message "loaded")
  (persp-mode)
  (persp-make-variable-persp-local 'mugu-directory)
  (customize-set-value 'persp-show-modestring nil))

(use-package mugu-project-utils
  :straight nil
  :defer
  :commands (persp-switch mugu-project-menu))

(provide 'mugu-project)
;;; mugu-project ends here
