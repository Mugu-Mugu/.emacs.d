;;; mugu-conf-window --- Configuration for window ui -*- lexical-binding: t -*-
;;; Commentary:

(require 'use-package)

(use-package mugu-window-defaults
  :straight nil
  :demand
  :config
  (mugu-window-defaults-activate))

(use-package mugu-window
  :straight nil
  :commands (mugu-window-menu)
  :general (:states 'motion "<backspace>" #'mugu-window-delete-next-side))

(use-package zoom
  :disabled
  :defer 5
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package golden-ratio
  :disabled
  :defer 5
  :config
  (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1))

(use-package mugu-wconf
  :straight nil
  :config (mugu-wconf-mode))


;;; Code:
;; -*- lexical-binding: t -*-
(provide 'mugu-conf-window)
;;; mugu-conf-window ends here
