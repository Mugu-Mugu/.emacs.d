;;; init --- Summary
;; tbc
;;; Commentary:

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "mugu-packages"))
(add-to-list 'load-path (concat user-emacs-directory "mugu-packages" "/languages"))
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
(use-package mugu-site-lisp :straight (:local-repo))     ;; site dependant configuration
(use-package mugu-hydra :straight (:local-repo))         ;; hydra extensions (add-head feature)
(use-package mugu-core :straight (:local-repo))          ;; base settings + some functions
(use-package mugu-directory-fix :straight (:local-repo)) ;; provide control on current working directory
(use-package mugu-menu :straight (:local-repo))          ;; provide main interaction menu + some menu functions

;;; personal or external optional features
(use-package mugu-sessions-persistance :straight (:local-repo))
(use-package mugu-modeline :straight (:local-repo))
(use-package mugu-interactive :straight (:local-repo))   ;; provide interactive selection (like ido/helm/ivy)
(use-package mugu-motion :straight (:local-repo))        ;; provide goto-{anything} features
(use-package mugu-completion :straight (:local-repo))
(use-package mugu-git :straight (:local-repo))
(use-package mugu-diff :straight (:local-repo))
(use-package mugu-evil :straight (:local-repo))
(use-package mugu-project :straight (:local-repo))
(use-package mugu-workspace :straight (:local-repo))
(use-package mugu-themes :straight (:local-repo))
(use-package mugu-lint :straight (:local-repo))
(use-package mugu-window :straight (:local-repo))
(use-package mugu-shell :straight (:local-repo))
(use-package mugu-bookmark :straight (:local-repo))
(use-package mugu-fold :straight (:local-repo))

;;; languages features
(use-package mugu-ada :straight (:local-repo))
(use-package mugu-lisp :straight (:local-repo))
(use-package mugu-py :straight (:local-repo))
(use-package mugu-org :straight (:local-repo))
(use-package mugu-rust :straight (:local-repo))
(use-package mugu-ruby :straight (:local-repo))

;;; mugu
(use-package mugu-keys :straight (:local-repo))
(use-package mugu-autoload :straight (:local-repo))

;;; gather all *global* binding to enforce coherency

(provide 'init)
;;; init ends here
