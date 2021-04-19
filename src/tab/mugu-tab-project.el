;;; mugu-tab-project --- Rules for automatic tab switching based on project -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-project)
(require 'mugu-tab)

(defun mugu-tab-project-attribution-rule (buffer)
  "A tab attribution rule based upon pinned status of the BUFFER."
  (mugu-project-name (mugu-project-root buffer)))

(defun mugu-tab-project-switch-to-current-project ()
  "Switch tab to current project and prompt for a file if needed."
  (let* ((project-tab-name projectile-project-name)
         (project-tab-already-loaded (mugu-tab-get-tab-with-name project-tab-name))
         (project-scratch-buffer (get-buffer-create (format "scratch (%s)" project-tab-name))))
    (if project-tab-already-loaded
        (mugu-tab-switch project-tab-name)
      (mugu-project-pin-buffer project-scratch-buffer project-tab-name)
      (display-buffer project-scratch-buffer))))

(defun mugu-tab-project--activate ()
  "."
  (mugu-tab-attribution-rule-add #'mugu-tab-project-attribution-rule)
  (setq projectile-switch-project-action #'mugu-tab-project-switch-to-current-project))

(defun mugu-tab-project--deactivate ()
  "."
  (custom-reevaluate-setting 'projectile-switch-project-action)
  (mugu-tab-attribution-rule-remove #'mugu-tab-project-attribution-rule))

(define-minor-mode mugu-tab-project-mode
  "A minor mode to activate automatic tab management based on buffer project."
  nil
  :global t
  :group 'mugu
  (if mugu-tab-project-mode
      (mugu-tab-project--activate)
    (mugu-tab-project--deactivate)))

(provide 'mugu-tab-project)
;;; mugu-tab-project ends here
