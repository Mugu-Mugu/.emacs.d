;;; init --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "mugu-packages"))
(add-to-list 'load-path (concat user-emacs-directory "mugu-packages" "/languages"))
(add-to-list 'load-path (concat user-emacs-directory "mugu-packages" "/wrappers"))
(add-to-list 'load-path (concat user-emacs-directory "src" "/pkg"))
(add-to-list 'load-path (concat user-emacs-directory "src"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "mugu-packages/" "mugu-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(defvar bootstrap-version "for straight bootstrap")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

(require 'straight)
(setq straight-use-package-by-default t)
(setq use-package-verbose t)

;;; external mandatory library and features
(use-package no-littering   :demand)
(use-package hydra          :demand)
(use-package undo-tree
  :custom (undo-tree-mode-lighter "")
  :straight (:host gitlab :repo "mwasilewski/undo-tree"))
(use-package evil           :demand)
(use-package key-chord      :demand)
(require 'mugu-lisp-libs)
(require 'mugu-general)

;;; personal mandatory library and features
(use-package mugu-site-lisp :straight nil)
(require 'mugu-hydra )
(require 'mugu-core )
(require 'mugu-directory-fix )
(require 'mugu-menu )

;;; personal or external optional features
(require 'mugu-sessions-persistance)
(require 'mugu-modeline)
(require 'mugu-interactive)
(require 'mugu-motion)
(require 'mugu-completion)
(require 'mugu-git)
(require 'mugu-diff)
(require 'mugu-evil)
(require 'mugu-project)
(require 'mugu-themes)
(require 'mugu-lint)
(require 'mugu-window)
(require 'mugu-shell)
(require 'mugu-bookmark)
(require 'mugu-fold)
(require 'mugu-search )
(require 'mugu-cosmetics)

;;; languages features
(require 'mugu-lang)

;;; mugu
(require 'mugu-keys)
(require 'mugu-autoload)

;; to move elsewhere
(use-package yaml-mode :defer)
(set-frame-font "DejaVu Sans Mono-9:bold")
(use-package web-mode :defer)
(use-package rainbow-delimiters :defer 10
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config)
(use-package expand-region
  :config
  (general-def '(motion visual)
    "RET" 'er/expand-region))

(general-def '(motion visual)
  "M-n" 'mc/mark-next-like-this)
(use-package wgrep :defer)

;;; gather all *global* binding to enforce coherency

(provide 'init)
;;; init ends here
