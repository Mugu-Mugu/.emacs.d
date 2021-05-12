;;; mugu-conf-keystone --- Summary
;; Those packages are hard dependcies to the rest of my configuration.
;; They may be customized further in anoter part but they are preloaded now to ensure
;; loading is not dependent on order.
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package lv :demand)

(use-package no-littering :demand)

(use-package hydra
  :demand
  :custom
  (hydra-look-for-remap t)
  :config
  ;; hack needed to solve flickering issue https://github.com/abo-abo/hydra/issues/349
  (setq hydra--work-around-dedicated nil))

;; consider removal
(use-package major-mode-hydra
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  (major-mode-hydra-separator "─"))

(use-package use-package-hydra
  :ensure t)

(use-package undo-tree
  :custom (undo-tree-mode-lighter "")
  :straight (:host gitlab :repo "mwasilewski/undo-tree"))

(use-package evil :demand)

(use-package general :demand)

(use-package ivy :demand)

(use-package key-chord
  :demand
  :config
  (key-chord-mode t)
  (setq key-chord-two-keys-delay 0.13))


(provide 'mugu-conf-keystone)
;;; mugu-conf-keystone ends here
