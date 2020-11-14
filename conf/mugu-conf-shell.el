;;; mugu-conf-shell --- Summary
;; tbc
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package mugu-shell
  :defer
  :straight nil
  :commands mugu-shell-menu shell
  :init
  (mugu-menu-register-mode-menu 'shell-mode #'mugu-shell-menu)
  :config
  (mugu-shell-activate-evil-conf))

(use-package term
  :defer
  :config
  (define-key term-raw-map [remap evil-paste-after] 'term-paste)
  (setq ansi-term-color-vector [term
                                term-color-black
                                term-color-red
                                term-color-green
                                term-color-yellow
                                term-color-blue
                                term-color-magenta
                                term-color-cyan
                                term-color-white]))

(use-package vterm
  :defer
  :commands vterm
  :init
  (add-to-list 'load-path "/home/david/.emacs.d/straight/build/vterm")
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (require 'mugu-vterm))

(use-package mugu-vterm
  :general
  (:states 'motion "&" #'mugu-vterm-toggle)
  :straight nil
  :commands mugu-vterm-switch
  :config
  (mugu-vterm-activate))

(provide 'mugu-conf-shell)
;;; mugu-conf-shell ends here
