;;; mugu-search --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package sos :defer :disabled)

(use-package sx :defer)

(use-package mugu-sx :defer
  :straight nil
  :hook
  (sx-question-list-mode . mugu-sx-menu-questions-list-delayed)
  :config
  (mugu-sx-configure-bindings))

(use-package google-this :defer)

(provide 'mugu-search)
;;; mugu-search ends here
