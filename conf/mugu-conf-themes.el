;;; mugu-conf-themes --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

;; NICE THEMES
(use-package noctilux-theme
  :disabled "in favor of doom modes and because it has some caveat with some mode"
  :demand
  :init (load-theme 'noctilux t))

(use-package doom-themes
  :demand
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-city-lights t))
  ;; very nice theme
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-vibrant t)
  ;; nice themes
  ;; (load-theme 'doom-palenight t)
  ;; (load-theme 'doom-moonlight t)
  ;; okeish themes
  ;; (load-theme 'doom-oceanic-next t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-sourcerer t)
  ;; (load-theme 'doom-wilmersdorf t))

(use-package zenburn-theme
  :demand t
  :disabled)

(use-package color-theme-sanityinc-tomorrow
  :disabled)

(use-package badger-theme
  :disabled)


;; AVERAGE THEMES
(use-package base16-theme
  :disabled)

(use-package solarized-theme
  :disabled
  :init (load-theme 'solarized-dark t))

(use-package spacegray-theme
  :disabled)

(use-package moe-theme
  :disabled
  :config
  (progn
    ;; Show highlighted buffer-id as decoration. (Default: nil)
    ;; Resize titles (optional).
    (setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
    (setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
    (setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
    ;; Choose a color for mode-line.(Default: blue)
    (load-theme 'moe-dark t)
    ;; Finally, apply moe-theme now.
    ;; Choose what you like, (moe-light) or (moe-dark)
    ))

(use-package monokai-theme
  :disabled
  :init (load-theme 'monokai t))

;; bad or unknown THEMES
(use-package aurora-theme
  :disabled)

(use-package afternoon-theme
  :disabled)

(use-package seti-theme
  :disabled)

(use-package tangotango-theme
  :disabled)

(use-package grandshell-theme
  :disabled)

(use-package alect-themes
  :disabled
  :init (load-theme 'alect-light t))

(use-package ample-theme
  :demand
  :disabled
  :init (load-theme 'ample t))

(provide 'mugu-themes)

(provide 'mugu-conf-themes)
;;; mugu-conf-themes ends here
