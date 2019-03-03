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


(defun newline-without-break-of-line ()
  "1. move to end of the line.
2. open new line and move to new line"
  (interactive)
  (end-of-line)
  (open-line 1)
  (right-char))

(provide 'mugu-lisp-libs)
;;; mugu-lisp-libs ends here
