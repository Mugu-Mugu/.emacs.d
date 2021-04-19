;;; mugu-window-utils --- Utilities related to window management -*- lexical-binding: t -*-
;;; Commentary:

(require 'mugu-buffer)
(require 'asoc)
(require 'lv)

;;; Code:
(defvar mugu-window-last-side-buffer-dismissed
  nil
  "Refer to the last side buffer that was dismissed.")

(defvar mugu-window-last-side-window-dismissed-params
  nil
  "Contains the last side window that was dismissed.")

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
  (add-to-list 'display-buffer-alist (apply #'mugu-window-generate-side-window-settings args)))

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

(defun mugu-window-delete-all-windows ()
  "Delete all window but the main one."
  (interactive)
  (let ((main-window (if (mugu-window-side-p)
                         (get-window-with-predicate (lambda (w) (not (mugu-window-side-p w))))
                       (selected-window))))
    (delete-other-windows main-window)))

(provide 'mugu-window-utils)
;;; mugu-window-utils ends here
