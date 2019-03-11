;;; mugu-conf-git --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package magit
  :defer
  :config
  (setq magit-save-repository-buffers 'dontask)
  (evil-set-initial-state 'git-commit-mode 'insert))

(use-package mugu-magit
  :straight nil
  :after magit
  :config
  (mugu-magit-enable-menus))

(provide 'mugu-conf-git)
;;; mugu-conf-git ends here
