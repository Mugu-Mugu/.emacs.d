;;; mugu-conf-temp --- Playground to test packages before integration or not
;; tbc
;;; Commentary:
(require 'use-package)

;;; Code:
;; to move elsewhere
(use-package yaml-mode :defer)
(use-package web-mode :defer)
(use-package restclient :defer)
(use-package docker :defer)

(use-package dockerfile-mode :defer
  :config
  (add-to-list 'load-path "/your/path/to/dockerfile-mode/")
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package emacs-everywhere :defer)

(use-package ox-pandoc
  :defer)

(use-package forge
  :defer)

(use-package github-review
  :defer)

(use-package nix-mode
  :mode "\\.nix\\'")

 (use-package direnv
 :config
 (direnv-mode))

;(use-package dap-mode
  ;; :defer)

(use-package org-transclusion
  :defer)

(provide 'mugu-conf-temp)
;;; mugu-conf-temp ends here
