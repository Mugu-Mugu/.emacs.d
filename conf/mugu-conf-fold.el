;;; mugu-conf-fold --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package yafolding
  :defer)

(use-package mugu-yafold
  :straight nil
  :general
  (:keymaps 'global
            :states 'motion
            :predicate '(not (eq major-mode 'org-mode))
            "<tab>" (general-key-dispatch #'yafolding-toggle-element
                      :timeout 0.15
                      "<tab>" 'mugu-yafold-main)))

(use-package outline
  :defer t
  :disabled
  :delight outline-minor-mode)

(use-package outshine
  :disabled
  :defer t
  :delight outshine-mode
  :hook  ((emacs-lisp-mode) . outshine-mode))

(use-package origami
  :defer t
  :disabled)

(use-package mugu-origami
  :straight nil
  :disabled)

(provide 'mugu-conf-fold)
;;; mugu-conf-fold ends here
