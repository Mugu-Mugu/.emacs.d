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
  :config
  (global-evil-mc-mode))

(provide 'mugu-conf-evil)
;;; mugu-conf-evil ends here
