;;; mugu-conf-fold --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package yafolding
  :disabled
  :defer
  :commands yafolding-toggle-element)

(use-package mugu-yafold
  :straight nil
  :disabled
  :commands mugu-yafold-main
  :general)

(use-package outline
  :defer t
  :delight outline-minor-mode)

(use-package outshine
  :disabled
  :defer t
  :delight outshine-mode
  :hook  (emacs-lisp-mode . outshine-mode))

(use-package origami
  :commands origami-mode origami-recursively-toggle-node
  :defer)

(use-package mugu-origami
  :straight nil
  :defer
  :commands mugu-origami-menu mugu-origami-toggle-node-and-activate
  :general
  (:keymaps 'global
            :states 'motion
            :predicate '(not (eq major-mode 'org-mode))
            "<tab>" (general-key-dispatch #'mugu-origami-toggle-node-and-activate
                      :timeout 0.22
                      "<tab>" 'mugu-origami-menu)))

(use-package lsp-origami
  :defer
  :hook (origami-mode-hook . lsp-origami-mode))

(provide 'mugu-conf-fold)
;;; mugu-conf-fold ends here
