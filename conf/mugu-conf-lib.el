;;; mugu-conf-lib --- Summary
;; Ensure presence of these lisp library
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package dash :defer)
(use-package s :defer)
(use-package ht :defer)
(use-package f :defer)
(use-package noflet :defer)
(use-package async :defer)
(use-package transient :config (transient-bind-q-to-quit))
(use-package request :defer)
(use-package asoc
  :straight (:host github :repo "troyp/asoc.el")
  :demand)

(provide 'mugu-conf-lib)
;;; mugu-conf-lib ends here
