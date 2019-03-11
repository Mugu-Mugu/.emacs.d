;;; mugu-conf-lib --- Summary
;; Ensure presence of these lisp library
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package dash :defer)
(use-package s :defer)
(use-package ht :defer)
(use-package noflet :defer)

(provide 'mugu-conf-lib)
;;; mugu-conf-lib ends here
