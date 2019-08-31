;;; mugu-conf-json --- Configuration for Json -*- lexical-binding: t -*-
;;; Commentary:
(require 'use-package)
(require 'mugu-menu)

;;; Code:
(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config
  (defmenu mugu-json-menu
    (:color blue :hint nil)
    ("f" json-mode-beautify "reformat")
    ("p" json-mode-show-path "display path")
    ("y" json-mode-kill-path "copy path"))
  (mugu-menu-register-mode-menu 'json-mode #'mugu-json-menu))

(provide 'mugu-conf-json)
;;; mugu-conf-json ends here
