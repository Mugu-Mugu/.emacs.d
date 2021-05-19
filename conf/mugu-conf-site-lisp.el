;;; mugu-conf-site-lisp --- Facilities for running site specific configuration -*- lexical-binding: t -*-
;;; Commentary:

(require 'f)
;;; Code:

(let ((site-lisp-dir (concat user-emacs-directory "site/")))
  (when (f-directory? site-lisp-dir)
    (-each (-filter 'f-readable? (f-glob "*.el" site-lisp-dir)) 'load-file)))

;; This file can be modified (to add local configuration for instance) but those modifications should not be pushed
;; run 'git update-index --assume-unchanged <this-file>' to ignore the modification
(provide 'mugu-conf-site-lisp)
;;; mugu-conf-site-lisp ends here
