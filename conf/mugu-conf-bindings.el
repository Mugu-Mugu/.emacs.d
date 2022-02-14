;;; mugu-conf-bindings --- Gather global bindings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package hercules
  :disabled "Does not play well with evil because the keymap does not account for normal state definitions which leads to a mismatch between bindings listed and reachable one."
  :general
  (:keymaps 'dired-mode-map
            [remap mugu-menu-call-mode-menu] #'ignore)
  :config
  (defun mugu-dired-menu () (interactive))
  (hercules-def
   :keymap 'dired-mode-map
   :transient t))

(use-package which-key
  :disabled "does not play will with evil either"
  ;; it is used as a library
  :custom
  (which-key-mode 0)
  (which-key-show-transient-maps t)
  (which-key-compute-remaps t))

(use-package which-key-posframe
  :disabled "it doesn't work"
  :after (which-key posframe)
  :config
  (which-key-posframe-mode)
  :custom
  (which-key-posframe-width 0.62)
  (which-key-posframe-height 0.2)
  (which-key-posframe-min-width 0.4)
  (which-key-posframe-min-height 0.1)
  (which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-center))

(provide 'mugu-conf-bindings)
;;; mugu-conf-bindings ends here
