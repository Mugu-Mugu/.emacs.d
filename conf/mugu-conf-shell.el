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
  :mode-hydra
  (vterm-mode
   (:title (with-faicon "terminal" "Vterm Mode") :color blue :hint nil)
   ("Vterm commands"
    (("C" vterm-clear))))
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no"))

(use-package mugu-vterm
  :straight nil
  :general
  (:states 'normal "&" #'mugu-vterm-toggle)
  :commands mugu-vterm-switch mugu-vterm-toggle
  :config
  (mugu-vterm-activate))

(use-package mugu-yasnippet
  :after (vterm yasnippet)
  :straight nil
  :general
  (:keymaps 'vterm-mode-map :states '(insert normal)
             "²" #'mugu-yasnippet-insert
             (general-chord "²²") #'mugu-yasnippet-menu))

(use-package mugu-vterm-snippet
  :straight nil
  :after (yasnippet vterm mugu-vterm)
  :config
  (mugu-vterm-snippet-mode))

(use-package evil-collection
  :after vterm
  :functions evil-collection-vterm-setup
  :config
  (evil-collection-vterm-setup))

(use-package mugu-tab
  :straight nil
  :after mugu-vterm
  :config
  (mugu-tab-tabify-display-buffer-alist-rules #'mugu-vterm-buffer-vterm-p))

(provide 'mugu-conf-shell)
;;; mugu-conf-shell ends here
