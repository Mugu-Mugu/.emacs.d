;;; mugu-tab-project --- Rules for automatic tab switching based on project -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-project)
(require 'mugu-tab)

(defvar mugu-tab-project-root nil "Stores the root of the project for the current tab.")

(defun mugu-tab-project-root ()
  "Consider the pinned project for the tab as project root."
  mugu-tab-project-root)

(defun mugu-tab-project-attribution-rule (buffer)
  "A tab attribution rule based upon pinned status of the BUFFER."
  (with-current-buffer buffer
    (and (mugu-project-root) (mugu-project-name (mugu-project-root)))))

(defun mugu-tab-project-switch-to-current-project ()
  "Switch tab to current project and prompt for a file if needed."
  ;; on switch projectile-project-name is set
  (mugu-tab-switch-or-create projectile-project-name)
  (unless mugu-tab-project-root
    (setq mugu-tab-project-root (mugu-project-by-name projectile-project-name))))

(defun mugu-tab-project-after-switch ()
  "."
  (unless
      (let ((buffer-root (mugu-project-root-of-buffer (current-buffer))))
        (and mugu-tab-project-root
             buffer-root
             (not (f-same? buffer-root mugu-tab-project-root))
             (with-temp-buffer
               (projectile-find-file))))))

(defun mugu-tab-project--save-project ()
  "Record current root."
  (unless mugu-tab-project-root
    (setq mugu-tab-project-root (mugu-project-root))))

(defun mugu-tab-project--activate ()
  "."
  (mugu-tab-make-variable-local 'mugu-tab-project-root)
  (mugu-tab-attribution-rule-add #'mugu-tab-project-attribution-rule)
  (add-to-list 'mugu-project-root-fallback-functions #'mugu-tab-project-root)
  (add-hook 'mugu-tab-before-switch-hook #'mugu-tab-project--save-project)
  (add-hook 'projectile-after-switch-project-hook #'mugu-tab-project-after-switch)
  (setq projectile-switch-project-action #'mugu-tab-project-switch-to-current-project))

(defun mugu-tab-project--deactivate ()
  "."
  (setq mugu-project-root-fallback-functions (delete #'mugu-tab-project-root mugu-project-root-fallback-functions))
  (custom-reevaluate-setting 'projectile-switch-project-action)
  (remove-hook 'mugu-tab-before-switch-hook #'mugu-tab-project--save-project)
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
