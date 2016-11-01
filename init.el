(add-to-list 'load-path (concat user-emacs-directory "site_specific"))
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/eyecandy"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/languages"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/hydra"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;;; external mandatory features
(use-package hydra     :demand :ensure)
(use-package evil      :demand :ensure)
(use-package key-chord :demand :ensure)

;;; personal mandatory features 
(use-package mugu-hydra)         ;; automatic hydra docstring generation
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

;;; languages features
(use-package mugu-ada)
(use-package mugu-lisp)
(use-package mugu-lispy)
(use-package mugu-org)

;;; gather all *global* binding to enforce coherency
(use-package mugu-keys)

;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "config/" "mugu-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)
