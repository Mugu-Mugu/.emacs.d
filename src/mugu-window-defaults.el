;;; mugu-window-defaults --- Define sane defaults for window -*- lexical-binding: t -*-
;;; Commentary:
(require 'mugu-window-utils)

;;; Code:
(defun mugu-window-defaults-activate ()
  "Define various sane defaults for window management/display."
  (winner-mode 1)
  (mugu-window-configure-side-window "\\*Help\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Warnings\\*" 'bottom 10)
  (mugu-window-configure-side-window "\\*lispy-help\\*" 'right 80)
  (mugu-window-configure-side-window "\\*lispy-message\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Apropos\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Backtrace\\*" 'bottom 0.2)
  (mugu-window-configure-side-window "\\*Shell Command Output\\*" 'bottom 0.5)
  (mugu-window-configure-side-window "\\*Messages\\*" 'top 0.3)

  (setq display-buffer-base-action
        (cons '(display-buffer-reuse-window display-buffer-same-window)
              '((inhibit-switch-frame . t)))))

(provide 'mugu-window-defaults)
;;; mugu-window-defaults ends here
