;;; Package --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package evil
  :demand
  :diminish
  :custom
  (evil-want-C-i-jump t)
  (evil-jumps-cross-buffers nil)
  (evil-overriding-maps nil)
  :config
  (evil-mode 1))

(use-package evil-surround
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\{ . ("{" . "}"))

                               (?\) . ("(" . ")"))
                               (?\] . ("[" . "]"))
                               (?\} . ("{" . "}"))

                               (?# . ("#{" . "}"))
                               (?b . ("(" . ")"))
                               (?B . ("{" . "}"))
                               (?> . ("<" . ">"))
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)
                               (?f . evil-surround-function)))
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))

(use-package evil-mc
  :diminish
  :defer
  :general (:states
            'motion
            "C-n" #'evil-mc-make-and-goto-next-match
            "C-p" #'evil-mc-make-and-goto-prev-match)
  :config
  (global-evil-mc-mode)
  (add-to-list 'evil-mc-incompatible-minor-modes 'lispy-mode)
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-lispy-mode))

(use-package mugu-mc
  :straight nil
  :after evil-mc
  :commands mugu-mc-menu
  :general
  (:states '(normal visual)
           "," #'mugu-mc-menu))

(provide 'mugu-conf-evil)
;;; mugu-conf-evil ends here
