;;; mugu-conf-vanilla --- Summary
;; Gather various settings for vanilla emacs
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package mugu-vanilla
  :straight nil
  :config
  (mugu-vanilla-set-perf-settings)
  (mugu-vanilla-set-startup-settings)
  (mugu-vanilla-set-encoding-settings)
  (mugu-vanilla-set-backup-settings)
  (mugu-vanilla-set-visual-settings)
  (mugu-vanilla-set-other-settings)
  (mugu-vanilla-set-evil-initial-states))

(use-package diminish)

(use-package delight)

(use-package whitespace
  :delight global-whitespace-mode
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (whitespace-style '(trailing))
  :config
  (global-whitespace-mode))

(use-package paren
  :delight
  :custom
  (show-paren-style 'mixed)
  :config
  (show-paren-mode))

(use-package mugu-directory
  :straight nil
  :config
  (add-hook 'find-directory-functions #'mugu-directory-find-file-try-cd))

(provide 'mugu-conf-vanilla)
;;; mugu-conf-vanilla ends here
