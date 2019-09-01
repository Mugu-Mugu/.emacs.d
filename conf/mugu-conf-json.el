;;; mugu-conf-json --- Configuration for Json -*- lexical-binding: t -*-
;;; Commentary:
(require 'use-package)
(require 'mugu-menu)

;;; Code:
(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config
  (require 'mugu-lang)
  (defmenu mugu-json-additional-menu
    (:color blue :hint nil)
    ("p" json-mode-show-path "display path")
    ("y" json-mode-kill-path "copy path"))
  (general-define-key
   :keymaps 'json-mode-map
   [remap mugu-menu-call-mode-menu] #'mugu-lang-menu
   [remap mugu-lang-additional-menu] #'mugu-json-additional-menu
   [remap mugu-lang-format-buffer] #'json-mode-beautify))

(provide 'mugu-conf-json)
;;; mugu-conf-json ends here
