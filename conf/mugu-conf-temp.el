;;; mugu-conf-temp --- Summary
;; tbc
;;; Commentary:
(require 'use-package)

;; to move elsewhere
(use-package yaml-mode :defer)
(use-package web-mode :defer)
(use-package wgrep :defer)
(use-package restclient :defer)
(use-package vterm
  :defer
  :init
  (add-to-list 'load-path "/home/david/.emacs.d/straight/build/vterm"))

;;; Code:

(provide 'mugu-conf-temp)
;;; mugu-conf-temp ends here
