;;; mugu-conf-text --- Global definition for text editions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package drag-stuff
  :hook (prog-mode . drag-stuff-global-mode)
  :custom
  (drag-stuff-except-modes '(org-mode emacs-lisp-mode minibuffer-inactive-mode))
  :general
  (:keymaps 'drag-stuff-mode-map
            "M-j" #'drag-stuff-down
            "M-k" #'drag-stuff-up
            "M-l" #'drag-stuff-right
            "M-h" #'drag-stuff-left))

(provide 'mugu-conf-text)
;;; mugu-conf-text ends here
