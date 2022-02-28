;;; mugu-org-wconf --- Gather org wconf rules -*- lexical-binding: t -*-
;;; Commentary:

(require 'mugu-window)
(require 'org-capture)

;;; Code:
(defun mugu-org-wconf-org-buffer-p (buffer-name &rest _)
  "Predicate indicating if BUFFER-NAME is a standard org-buffer."
  (with-current-buffer buffer-name
    (and (not org-capture-mode) (eq major-mode 'org-mode))))

(defun mugu-org-wconf--configure (is-activation)
  "Activate wconf if IS-ACTIVATION is true.  Deactivate otherwise."
  (let ((func (if is-activation #'mugu-window-configure-side-window #'mugu-window-remove-display-rule)))
    (funcall func #'mugu-org-wconf-org-buffer-p 'bottom 0.5 0)
    (funcall func "\\*org-roam\\*" 'bottom 0.2 1)
    (funcall func "^CAPTURE.*.org" 'bottom 0.2)
    (funcall func "\\*Org Note\\*" 'bottom 0.2)))

(define-minor-mode mugu-org-wconf-mode
  "Handle display configuration for org files."
  :global t
  :group 'mugu
  (if mugu-org-wconf-mode
      (mugu-org-wconf--configure 'activation)
    (mugu-org-wconf--configure nil)))

(provide 'mugu-org-wconf)
;;; mugu-org-wconf ends here
