;;; mugu-conf-diff --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package ediff
  :defer
  :config
  ;; ???? whtat those vars?
  (customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
  (customize-set-variable 'ediff-split-window-function 'split-window-horizontally)
  (customize-set-variable 'ediff-diff-options "-w")
  (defun mugu-ediff-hook ()
    (ediff-setup-keymap)
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))
  (add-hook 'ediff-mode-hook 'mugu-ediff-hook))

(provide 'mugu-conf-diff)
;;; mugu-conf-diff ends here
