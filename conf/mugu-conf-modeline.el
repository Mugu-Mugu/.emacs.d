;;; mugu-conf-modeline --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package telephone-line)

(use-package mugu-telephone-line
  :after telephone-line
  :straight nil
  :config
  (mugu-telephone-line-configure)
  (mugu-telephone-line-set-nocti-theme)
  (telephone-line-mode 1))

(use-package spaceline
  :demand
  :disabled
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; (spaceline-spacemacs-theme)
  (setq spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t))


(provide 'mugu-conf-modeline)
;;; mugu-conf-modeline ends here