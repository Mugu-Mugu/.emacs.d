;;; mugu-conf-search --- Summary
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

(use-package google-this
  :disabled
  "It sucks, same as webjump: it forgots that interactive is not everything...
can't search without inserting whatever shit is at point."
  :defer)

(use-package engine-mode :defer
  :disabled "counsel search does this better.")

(use-package mugu-search
  :disabled "counsel search does this better"
  :straight nil
  :commands engine/search-google)

(provide 'mugu-conf-search)
;;; mugu-conf-search ends here
