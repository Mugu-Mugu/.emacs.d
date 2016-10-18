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

;;; main emacs config here
(use-package mugu-core)
(use-package mugu-sessions-persistance)
(use-package mugu-hydra)
(use-package mugu-modeline)
(use-package mugu-interactive)
(use-package mugu-motion)
(use-package mugu-completion)
(use-package mugu-lisp)
(use-package mugu-lispy :disabled)
(use-package mugu-directory-fix)
(use-package mugu-git)
(use-package mugu-diff)
(use-package mugu-evil)
(use-package mugu-project)
(use-package mugu-workspace)
(use-package mugu-themes)
(use-package mugu-site-lisp)
(use-package mugu-ada)
(use-package mugu-lint)

;; git log single file
;; better magit support
;; bind ace link
;; smart mx with only selected command
;; smart save
;; bind grep
;; better counsel occur support
;; improve company configuration
;; setup org
;; setup ada
;; retry  lipsy
;; better workspace integration in modeline
;; try spaceline?



;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "config/" "mugu-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(add-hook 'ada-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(provide 'init)
