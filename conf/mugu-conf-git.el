;;; mugu-conf-git --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package magit
  :defer
  :config
  (setq magit-save-repository-buffers 'dontask)
  (evil-set-initial-state 'git-commit-mode 'insert)
  :custom
  (vc-handled-backends nil))

(use-package mugu-magit
  :straight nil
  :after magit
  :config
  (mugu-magit-enable-menus)
  (mugu-magit-configure))

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

(use-package diff-hl
  :disabled
  :defer
  :after magit
  :config
  (global-diff-hl-mode))

(use-package git-gutter
  :defer
  :after magit
  :config
  (global-git-gutter-mode +1))

(use-package mugu-git-gutter
  :straight nil
  :commands mugu-git-gutter-menu)

(use-package vdiff
  :disabled
  :defer)

(use-package vdiff-magit
  :after magit
  :disabled
  :config
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package forge
  :disabled
  :after magit)

(provide 'mugu-conf-git)
;;; mugu-conf-git ends here
