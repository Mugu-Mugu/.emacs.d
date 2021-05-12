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

(defvar mugu-window-last-side-buffer-dismissed
  nil
  "Refer to the last side buffer that was dismissed.")

(defvar mugu-window-last-side-window-dismissed-params
  nil
  "Contains the last side window that was dismissed.")

(defvar mugu-window-display-rules-added-hook
  (list)
  "Hook triggered after a new display rule have been added.")

(defun mugu-window-generate-side-window-settings (buffer-name-or-predicate direction size &optional dont-bury)
  "Create a `display-buffer' rule.
The rule will be assigned to BUFFER-NAME-OR-PREDICATE.
The side window will be open from DIRECTION which should be top, bottom, right
or left.
The SIZE of the window determine how much room it takes.  It must be a float
between 0.0 and 1.0.
If DONT-BURY is not nil, it will not bury the associated buffer on fast close.
Side window not declared by this mean won't be managed."
  (let ((height-or-width (if (or (eq direction 'top)
                                  (eq direction 'bottom))
                              'window-height
                           'window-width)))
    `(,buffer-name-or-predicate
      (mugu-window-display-in-side display-buffer-in-side-window display-buffer-same-window display-buffer-use-some-window)
      (side . ,direction)
      (slot . 1)
      (,height-or-width . ,size)
      (inhibit-switch-frame . t)
      (window-parameters (dont-bury ,dont-bury)
                         (mugu-window ,t)))))

(defun mugu-window-configure-side-window (&rest args)
  "Add a rule to `display-buffer-alist' to display a buffer as side window.
For ARGS definition, refer to `mugu-window-generate-side-window-settings'."
  (add-to-list 'display-buffer-alist (apply #'mugu-window-generate-side-window-settings args))
  (run-hooks 'mugu-window-display-rules-added-hook))

(defun mugu-window-configure-normal-window (condition)
  "Add a CONDITION to `display-buffer-alist' to display a buffer as a normaly.
It will defines a policy to reuse window in a sane and coherent way."
  (add-to-list 'display-buffer-alist
               `(,condition . ((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-reuse-mode-window display-buffer-same-window))))
  (run-hooks 'mugu-window-display-rules-added-hook))

(defun mugu-window-remove-display-rule (condition &rest _)
  "Remove all display rules matching CONDITION."
  (setq display-buffer-alist (--reject (equal (car it) condition) display-buffer-alist)))

(defun mugu-window-side-p (&optional window)
  "Predicate determining if WINDOW is a side window.
Default to `selected-window'."
  (window-parameter (or window (selected-window)) 'window-side))

(defun mugu-window-side-managed-p (&optional window)
  "Predicate determining if WINDOW is a a side window managed by this package."
  (let ((window (or window (selected-window))))
    (and (mugu-window-side-p window) (window-parameter window 'mugu-window))))

(defun mugu-window-bury-buffer-delete-window (window)
  "Delete WINDOW and bury its buffer."
  (setq mugu-window-last-side-buffer-dismissed (window-buffer window))
  (setq mugu-window-last-side-window-dismissed-params (window-parameters window))
  (unless (window-parameter window 'dont-bury) (bury-buffer (window-buffer window)))
  (delete-window window))

(defun mugu-window-delete-or-toggle-side (&optional with-focus)
  "Delete or toggle the next side window.
WITH-FOCUS will also select it."
  (interactive)
  (let ((next-side-window (get-window-with-predicate 'mugu-window-side-managed-p)))
    (if next-side-window
        (mugu-window-bury-buffer-delete-window next-side-window)
      (if mugu-window-last-side-buffer-dismissed
          (progn
            (mugu-window-display-in-side
             mugu-window-last-side-buffer-dismissed
             mugu-window-last-side-window-dismissed-params)
            (when with-focus (switch-to-buffer mugu-window-last-side-buffer-dismissed)))

        (message "There is no side window open.")))))

(defun mugu-window-last-p ()
  "Predicate determining if current window is the last one."
  (= 1 (length (--reject (eq it lv-wnd) (window-list)))))

(defun mugu-window-delete ()
  "Delete current window unless its the last one."
  (interactive)
  (if (mugu-window-last-p)
      (message "can not delete last live window")
    (delete-window)))

(defun mugu-window-display-in-side (buffer alist)
  "Behave as `display-buffer-in-side-window' but hook some parameters.
BUFFER and ALIST parameters have the same semantics as in other `display-buffer' methods."
  (let* ((additional-window-parameters (asoc-merge '((mugu-window . t)) (asoc-get alist 'window-parameters) alist))
         (modified-alist (asoc-merge alist `((window-parameters . ,additional-window-parameters)))))
    (display-buffer-in-side-window buffer modified-alist)))

(defun mugu-window--delete-other-windows-advice (original-delete-other-windows &rest args)
  "Saner replacement of `delete-other-windows' w.r.t side window.
ARGS are forwarded as is to ORIGINAL-DELETE-OTHER-WINDOWS."
  (interactive)
  (let ((main-window (if (mugu-window-side-p)
                         (get-window-with-predicate (lambda (w) (not (mugu-window-side-p w))))
                       (selected-window))))
    (message "main window %s side %s" main-window (mugu-window-side-p main-window))
    (apply original-delete-other-windows main-window args)))

(defun mugu-window-defaults--activate ()
  "Define various sane defaults for window management/display."
  (winner-mode 1)
  (advice-add 'delete-other-windows :around #'mugu-window--delete-other-windows-advice)
  (mugu-window-configure-side-window "\\*Help\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Warnings\\*" 'bottom 10)
  (mugu-window-configure-side-window "\\*lispy-help\\*" 'right 80)
  (mugu-window-configure-side-window "\\*lispy-message\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Apropos\\*" 'right 80)
  (mugu-window-configure-side-window "\\*Backtrace\\*" 'bottom 0.2)
  (mugu-window-configure-side-window "\\*Shell Command Output\\*" 'bottom 0.5)
  (mugu-window-configure-side-window "\\*Messages\\*" 'top 0.3)
  (setq display-buffer-base-action
        (cons mugu-window-base-display-action
              '((inhibit-switch-frame . t)))))

(defun mugu-window-defaults--deactivate ()
  "."
  (winner-mode -1)
  (advice-remove 'delete-other-windows #'mugu-window--delete-other-windows-advice))

(define-minor-mode mugu-window-mode
  "A minor mode to provide saner default/hacks for basic window display."
  nil
  :global t
  :group 'mugu
  (if mugu-window-mode
      (mugu-window-defaults--activate)
    (mugu-window-defaults--deactivate)))

(provide 'mugu-window)
;;; mugu-window ends here
