;;; mugu-conf-keystone --- Summary
;; Those packages are hard dependcies to the rest of my configuration.
;; They may be customized further in anoter part but they are preloaded now to ensure
;; initial loading is not dependent on order.
;;; Commentary:

;;; Code:
(require 'use-package)


(use-package no-littering :demand)

(use-package hydra :demand)

(use-package undo-tree
  :custom (undo-tree-mode-lighter "")
  :straight (:host gitlab :repo "mwasilewski/undo-tree"))

(use-package evil :demand)

(use-package general :demand)

(use-package key-chord
  :demand
  :config
  (key-chord-mode t)
  (setq key-chord-two-keys-delay 0.1))


(provide 'mugu-conf-keystone)
;;; mugu-conf-keystone ends here
