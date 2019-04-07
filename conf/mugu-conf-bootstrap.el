;;; mugu-conf-bootstrap --- Summary
;; Gather glue code required to bootstrap my emacs configuration
;;; Commentary:

;;; Code:

(if (version< emacs-version "25.1")
  (error "This Emacs version is too old %s (required > 25.1)" emacs-version))

;; * begin:
(defvar bootstrap-version "for straight bootstrap")
(defvar use-package-verbose)
(defvar straight-use-package-by-default)

(defun mugu-bootstrap-add-dir (dir)
  "Add DIR and its descendant to the load path.
DIR is expected to be under `user-emacs-directory'."
  (let ((default-directory (concat user-emacs-directory dir)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(defun mugu-bootstrap-load-straight ()
  "Load straight config."
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

  (require 'straight)
  (require 'use-package)

  (setq straight-use-package-by-default t)
  (setq use-package-verbose t)
  (setq straight-recipes-gnu-elpa-use-mirror t))

(defun mugu-bootstrap-activate ()
  "Start the boot sequence."
  (mugu-bootstrap-add-dir "src")
  (mugu-bootstrap-add-dir "conf")
  (mugu-bootstrap-load-straight))

(provide 'mugu-conf-bootstrap)
;;; mugu-conf-bootstrap ends here
