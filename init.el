;;; init --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(require 'mugu-conf-bootstrap (concat user-emacs-directory "conf/mugu-conf-bootstrap.el"))
(mugu-bootstrap-activate)

;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "conf/" "mugu-conf-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(require 'mugu-conf-keystone)
(require 'mugu-conf-lib)
(require 'mugu-conf-vanilla)
(require 'mugu-conf-evil)
(require 'mugu-conf-lisp)
(require 'mugu-conf-session)
(require 'mugu-conf-modeline)
(require 'mugu-conf-project)
(require 'mugu-conf-cosmetics)
(require 'mugu-conf-motion)
(require 'mugu-conf-interactive)
(require 'mugu-conf-completion)
(require 'mugu-conf-git)
(require 'mugu-conf-diff)
(require 'mugu-conf-lang)
(require 'mugu-conf-themes)
(require 'mugu-conf-shell)
(require 'mugu-conf-lint)
(require 'mugu-conf-search)
(require 'mugu-conf-org)
(require 'mugu-conf-temp)
(require 'mugu-conf-ruby)
(require 'mugu-conf-javascript)
(require 'mugu-conf-python)

(require 'mugu-site-lisp)
(require 'mugu-window)
(require 'mugu-fold)

;;; languages features
(require 'mugu-ada)
(require 'mugu-rust)

;;; mugu
(require 'mugu-keys)

(provide 'init)
;;; init ends here
