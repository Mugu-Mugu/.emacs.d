;;; mugu-motion --- Summary
;; tbc
;;; Commentary:
(require 'mugu-core)
(require 'evil)

;;; Code:
(use-package avy
  :defer
  :bind
  (:map evil-normal-state-map
        ("s" . avy-goto-char-2)
        ("S" . avy-goto-line)
        ("t" . evil-mugu-motion-t)
        ("T" . evil-mugu-motion-T)
        ("f" . evil-mugu-motion-f)
        ("F" . evil-mugu-motion-F))
  :config
  (require 'mugu-motion-utils)
  (setq avy-case-fold-search t)
  (setq avy-all-windows nil)
  (setq avy-background t)
  (setq avy-style 'at-full)
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-timeout-seconds 0.3)
  (evil-define-key 'motion evil-motion-state-map
    "s" 'avy-goto-char-2
    "S" 'avy-goto-line
    "t" 'evil-mugu-motion-t
    "T" 'evil-mugu-motion-T
    "f" 'evil-mugu-motion-f
    "F" 'evil-mugu-motion-F))

(use-package mugu-motion-utils
  :defer
  :straight (:local-repo))

(use-package ace-window
  :defer
  :config (setq aw-keys '(?a ?z ?e ?r ?t)))

(use-package ace-link
  :defer)

(provide 'mugu-motion)
;;; mugu-motion ends here
