;;; mugu-conf-motion --- Summary
;; tbc
;;; Commentary:
(require 'evil)
(require 'general)
(require 'use-package)

;;; Code:
(use-package avy
  :defer
  :config
  (set-face-attribute 'avy-goto-char-timer-face nil :inherit 'lazy-highlight)
  (setq avy-case-fold-search t)
  (setq avy-all-windows t)
  (setq avy-background t)
  (setq avy-style 'at-full)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?m ?z ?e ?r ?t ?u ?i ?o ?p ?v))
  (setq avy-timeout-seconds 0.5)
  (general-def 'motion
    "F" 'evil-avy-goto-line
    "f" 'evil-avy-goto-char-2))

(use-package mugu-motion
  :after avy
  :straight nil)

(use-package ace-window
  :defer
  :config (setq aw-keys '(?a ?e ?r ?t)))

(use-package ace-link :defer)

(provide 'mugu-conf-motion)
;;; mugu-conf-motion ends here
