;;; init --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-conf-bootstrap (concat user-emacs-directory "conf/mugu-conf-bootstrap.el"))
(mugu-bootstrap-activate)

;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "conf/" "mugu-conf-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(require 'mugu-conf-use-package-ext)
(require 'mugu-conf-keystone)
(require 'mugu-conf-lib)
(require 'mugu-conf-vanilla)
(require 'mugu-conf-evil)
(require 'mugu-conf-bindings)
(require 'mugu-conf-help)
(require 'mugu-conf-session)
(require 'mugu-conf-window)
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
(require 'mugu-conf-site-lisp)
(require 'mugu-conf-fold)
(require 'mugu-conf-writing)
(require 'mugu-conf-slack)
(require 'mugu-conf-misc)

;;; languages features
(require 'mugu-conf-lisp)
(require 'mugu-conf-javascript)
(require 'mugu-conf-python)
(require 'mugu-conf-ruby)
(require 'mugu-conf-xml)
(require 'mugu-conf-json)
(require 'mugu-conf-sql)
(require 'mugu-conf-rust)
(require 'mugu-conf-plantuml)
(require 'mugu-ada)


;;; mugu
(require 'mugu-keys)

(server-start)

(provide 'init)
;;; init ends here
;; (put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
