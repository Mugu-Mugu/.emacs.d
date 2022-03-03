;;; mugu-window --- Basic settings for vanilla emacs window -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'hydra)
(require 'winner)
(require 'use-package)
(require 'ace-window)
(require 'asoc)
(require 'lv)

;;; Code:
(defconst mugu-window-base-display-action
  (list #'display-buffer-reuse-window
        #'display-buffer-in-previous-window
        #'display-buffer-reuse-mode-window
        #'display-buffer-same-window
        #'display-buffer-use-some-window)
  "A list of functions to specifies base display action.")

(defun mugu-window-generate-side-window-settings (buffer-name-or-predicate direction size &optional slot)
  "Create a `display-buffer' rule.
The rule will be assigned to BUFFER-NAME-OR-PREDICATE.
The side window will be open from DIRECTION which should be top, bottom, right
or left.
The SIZE of the window determine how much room it takes.  It must be a float
between 0.0 and 1.0."
  (let ((slot (or slot 0))
        (height-or-width (if (or (eq direction 'top)
                                 (eq direction 'bottom))
                              'window-height
                           'window-width)))
    `(,buffer-name-or-predicate
      (display-buffer-in-side-window)
      (side . ,direction)
      (slot . ,slot)
      (,height-or-width . ,size)
      (inhibit-switch-frame . t))))

(defun mugu-window-configure-side-window (&rest args)
  "Add a rule to `display-buffer-alist' to display a buffer as side window.
For ARGS definition, refer to `mugu-window-generate-side-window-settings'."
  (add-to-list 'display-buffer-alist
               (apply #'mugu-window-generate-side-window-settings args)
               'append))

(defun mugu-window-configure-normal-window (condition)
  "Add a CONDITION to `display-buffer-alist' to display a buffer as a normaly.
It will defines a policy to reuse window in a sane and coherent way."
  (add-to-list 'display-buffer-alist
               `(,condition . ((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-reuse-mode-window display-buffer-same-window)))
               'append))

(defun mugu-window-side-p (&optional window)
  "Predicate determining if WINDOW is a side window.
Default to `selected-window'."
  (window-parameter (or window (selected-window)) 'window-side))

(defun mugu-window-delete-all-side-windows ()
  "."
  (interactive)
  (when (get-window-with-predicate 'mugu-window-side-p)
    (window-toggle-side-windows)))

(defun mugu-window-non-side-windows ()
  "."
  (--reject (mugu-window-side-p it) (window-list-1)))

(defun mugu-window-remove-display-rule (condition &rest _)
  "Remove all display rules matching CONDITION."
  (setq display-buffer-alist (--reject (equal (car it) condition) display-buffer-alist)))

(defun mugu-window-before-delete-other-windows (&rest _args)
  "Saner replacement of `delete-other-windows' w.r.t side window.
The original method was failing when called on side window.  This is an issue
because this is not a sane default and a lot of org window configuration depends
on this method causing all sort of problem with my side windows."
  (when (mugu-window-side-p)
    (select-window (-first-item (mugu-window-non-side-windows)))))

(defun mugu-window-defaults--activate ()
  "Define various sane defaults for window management/display."
  (winner-mode 1)
  (advice-add 'delete-other-windows :before #'mugu-window-before-delete-other-windows)
  (mugu-window-configure-side-window "\\*Help\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Warnings\\*" 'bottom 10)
  (mugu-window-configure-side-window "\\*lispy-help\\*" 'right 80)
  (mugu-window-configure-side-window "\\*lispy-message\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Apropos\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Backtrace\\*" 'bottom 0.2)
  (mugu-window-configure-side-window "\\*Shell Command Output\\*" 'bottom 0.5)
  (mugu-window-configure-side-window "\\*grep\\*" 'top 0.3)
  (mugu-window-configure-side-window "\\*Messages\\*" 'top 0.3)
  (setq display-buffer-base-action
        (cons mugu-window-base-display-action
              '((inhibit-switch-frame . t)))))

(defun mugu-window-defaults--deactivate ()
  "."
  (advice-remove 'delete-other-windows #'mugu-window-before-delete-other-windows)
  (winner-mode -1))

(define-minor-mode mugu-window-mode
  "A minor mode to provide saner default/hacks for basic window display."
  nil
  :global t
  :keymap (make-sparse-keymap)
  :group 'mugu
  (if mugu-window-mode
      (mugu-window-defaults--activate)
    (mugu-window-defaults--deactivate)))

(provide 'mugu-window)
;;; mugu-window ends here
(defun mugu-window--delete-other-windows-advice (original-delete-other-windows &rest args)
  "Saner replacement of `delete-other-windows' w.r.t side window.
ARGS are forwarded as is to ORIGINAL-DELETE-OTHER-WINDOWS."
  (interactive)
  (let ((main-window (if (mugu-window-side-p)
                         (get-window-with-predicate (lambda (w) (not (mugu-window-side-p w))))
                       (selected-window))))
    (message "main window %s side %s" main-window (mugu-window-side-p main-window))
    (apply original-delete-other-windows main-window args)))
