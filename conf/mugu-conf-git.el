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

(use-package git-timemachine
  :defer
  :straight (:host gitlab :repo "pidu/git-timemachine"))

(use-package mugu-git-tm
  :straight nil
  :commands mugu-git-tm-menu-or-activate
  :config
  (mugu-git-tm-configure))

(use-package git-messenger
  :defer
  :custom
  (git-messenger:use-magit-popup t))

(provide 'mugu-conf-git)
;;; mugu-conf-git ends here
