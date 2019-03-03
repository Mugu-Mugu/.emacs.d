;;; mugu-motion --- Summary
;; tbc
;;; Commentary:
(require 'mugu-core)
(require 'evil)
(require 'general)

;;; Code:
(use-package avy
  :defer
  :config
  (require 'mugu-motion-utils)
  (setq avy-case-fold-search t)
  (setq avy-all-windows nil)
  (setq avy-background t)
  (setq avy-style 'at-full)
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-timeout-seconds 0.3)
  (general-def 'motion
    "F" 'evil-avy-goto-line
    "f" 'evil-avy-goto-char-2))

(use-package mugu-motion-utils
  :defer
  :straight nil)

(use-package ace-window
  :defer
  :config (setq aw-keys '(?a ?z ?e ?r ?t)))

(use-package ace-link
  :defer)

(provide 'mugu-motion)
;;; mugu-motion ends here
