;;; mugu-window-utils --- Utilities related to window management -*- lexical-binding: t -*-
;;; Commentary:

(require 'mugu-buffer)

;;; Code:
(defvar mugu-window-last-side-buffer-dismissed
  nil
  "Refer to the last side buffer that was dismissed.")

(defun mugu-window-configure-side-window (buffer-name-or-predicate direction size &optional dont-bury)
  "Add a rule to `display-buffer-alist' to display a buffer as side window.
The rule will be assigned to BUFFER-NAME-OR-PREDICATE.
The side window will be open from DIRECTION which should be top, bottom, right
or left.
The SIZE of the window determine how much room it takes.  It must be a float
between 0.0 and 1.0.
If DONT-BURY is not nil, it will not bury the associated buffer on fast close.
Side window not declared by this mean won't be managed."
  (let* ((height-or-width (if (or (eq direction 'top)
                                  (eq direction 'bottom))
                              'window-height
                            'window-width))
         (display-params `(,buffer-name-or-predicate
                           (display-buffer-in-side-window display-buffer-same-window display-buffer-use-some-window)
                           (side . ,direction)
                           (slot . 1)
                           (,height-or-width . ,size)
                           (inhibit-switch-frame . t)
                           (window-parameters (dont-bury ,dont-bury)
                                              (mugu-window ,t)))))
    (add-to-list 'display-buffer-alist display-params)))

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
  (unless (window-parameter window 'dont-bury) (bury-buffer (window-buffer window)))
  (delete-window  window))

(defun mugu-window-delete-next-side ()
  "Delete the next side window."
  (interactive)
  (let ((next-side-window (get-window-with-predicate 'mugu-window-side-managed-p)))
    (if next-side-window
        (mugu-window-bury-buffer-delete-window next-side-window)
      (if mugu-window-last-side-buffer-dismissed (mugu-buffer-switch mugu-window-last-side-buffer-dismissed)
        (message "There is no side window open.")))))

(defun mugu-window-delete-all-windows ()
  "Delete all window but the main one."
  (interactive)
  (let ((main-window (if (mugu-window-side-p)
                         (get-window-with-predicate (lambda (w) (not (mugu-window-side-p w))))
                       (selected-window))))
    (delete-other-windows main-window)))

(provide 'mugu-window-utils)
;;; mugu-window-utils ends here
