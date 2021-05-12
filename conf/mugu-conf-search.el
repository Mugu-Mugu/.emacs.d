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

(use-package counsel-web
  :defer
  :general
  ([remap mugu-feature-search-google] #'counsel-web-suggest
   [remap mugu-feature-search-google-at-point] #'counsel-web-thing-at-point)
  :custom
  (counsel-web-search-action #'browse-url-default-browser)
  (counsel-web-engine 'duckduckgo))

(use-package mugu-org-link
  :after org
  :straight nil
  :config
  (mugu-org-link-activate-ivy-actions)
  (org-link-set-parameters "http" :complete #'mugu-org-link-http-complete)
  (org-link-set-parameters "https" :complete #'mugu-org-link-http-complete))


(provide 'mugu-conf-search)
;;; mugu-conf-search ends here
