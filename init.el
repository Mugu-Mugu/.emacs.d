(add-to-list 'load-path (concat user-emacs-directory "mugu-packages"))
(add-to-list 'load-path (concat user-emacs-directory "mugu-packages" "/languages"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-user-dir (concat user-emacs-directory "/my-melpa-packages")) 

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "mugu-packages/" "mugu-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;;; external mandatory library and features
(use-package no-littering :demand :ensure)
(use-package hydra     :demand :ensure)
(use-package evil      :demand :ensure)
(use-package key-chord :demand :ensure)

;;; personal mandatory library and features 
(use-package mugu-hydra)         ;; hydra extensions (add-head feature)
(use-package mugu-core)          ;; base settings + some functions
(use-package mugu-directory-fix) ;; provide control on current working directory
(use-package mugu-menu)          ;; provide main interaction menu + some menu functions

;;; personal or external optional features
(use-package mugu-sessions-persistance)
(use-package mugu-modeline)
(use-package mugu-interactive)   ;; provide interactive selection (like ido/helm/ivy)
(use-package mugu-motion)        ;; provide goto-{anything} features
(use-package mugu-completion)
(use-package mugu-git)
(use-package mugu-diff)
(use-package mugu-evil)
(use-package mugu-project)       
(use-package mugu-workspace)
(use-package mugu-themes)
(use-package mugu-site-lisp)
(use-package mugu-lint)
(use-package mugu-window)
(use-package mugu-shell)
(use-package mugu-bookmark)

;;; languages features
(use-package mugu-ada)
(use-package mugu-lisp)
(use-package mugu-org)

;;; gather all *global* binding to enforce coherency
(use-package mugu-keys)


(provide 'init)
