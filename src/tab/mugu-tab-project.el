;;; mugu-tab-project --- Rules for automatic tab switching based on project -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-project)
(require 'mugu-tab)

(defvar mugu-tab-project-root nil "Stores the root of the project for the current tab.")
(defvar mugu-tab-project-default-buffer-name nil "Stores the name of the default project buffer.")

(defun mugu-tab-project-root ()
  "Consider the pinned project for the tab as project root."
  mugu-tab-project-root)

(defun mugu-tab-project-attribution-rule (buffer)
  "A tab attribution rule based upon pinned status of the BUFFER."
  (with-current-buffer buffer
    (and (mugu-project-root) (mugu-project-name (mugu-project-root)))))

(defun mugu-tab-project-change-tab ()
  "."
  (mugu-tab-switch-or-create projectile-project-name))

(defun mugu-tab-project-fix-wconf-maybe ()
  "When switching to a new project, there may be remnant of the old wconf.
This ensure a clean wconf if this is the case.
This can't be one on the tab change because the process happens in a
temporary buffer."
  (unless (and mugu-tab-project-root
               (--select (equal (mugu-project-root-of-buffer (window-buffer it))
                                mugu-tab-project-root)
                         (window-list-1)))
    (switch-to-buffer (-first-item (mugu-project-buffers mugu-tab-project-root)))
    (delete-other-windows)))

(defun mugu-tab-project-init ()
  "."
  (let* ((project-name (mugu-tab-current-tab-name))
         (project-root (mugu-project-by-name project-name))
         (default-buffer-name (format "scratch (%s)" project-name))
         (default-buffer (get-buffer-create default-buffer-name)))
    (setq mugu-tab-project-default-buffer-name default-buffer-name)
    (mugu-tab-pin-buffer default-buffer projectile-project-name)
    (when project-root
      (mugu-project-pin-buffer default-buffer project-root)
      (setq mugu-tab-project-root project-root))))

(defun mugu-tab-project--activate ()
  "."
  (mugu-tab-make-variable-local 'mugu-tab-project-root)
  (mugu-tab-attribution-rule-add #'mugu-tab-project-attribution-rule)
  (add-to-list 'mugu-project-root-fallback-functions #'mugu-tab-project-root)
  (add-hook 'mugu-tab-after-creation-hook #'mugu-tab-project-init)
  (add-hook 'projectile-after-switch-project-hook #'mugu-tab-project-fix-wconf-maybe)
  (setq projectile-switch-project-action #'mugu-tab-project-change-tab))

(defun mugu-tab-project--deactivate ()
  "."
  (setq mugu-project-root-fallback-functions (delete #'mugu-tab-project-root mugu-project-root-fallback-functions))
  (custom-reevaluate-setting 'projectile-switch-project-action)
  (remove-hook 'mugu-tab-after-creation-hook #'mugu-tab-project-init)
  (remove-hook 'projectile-after-switch-project-hook #'mugu-tab-project-fix-wconf-maybe)
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
