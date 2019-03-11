;;; mugu-lisp-libs --- Summary
;; gather use packages for external libraries needed for my packages
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package dash
  :demand)

(use-package s
  :demand)

(use-package ht
  :demand)

(use-package noflet)

(provide 'mugu-lisp-libs)
;;; mugu-lisp-libs ends here
