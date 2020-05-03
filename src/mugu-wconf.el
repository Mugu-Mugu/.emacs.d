;;; mugu-wconf --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
(require 'ht)
(require 'dash)
(require 'asoc)
(require 'mugu-window-utils)

;;; Code:
(defvar mugu-wconf-map (ht-create) "Gather saved window configuration.")
(defvar mugu-wconf-rules (asoc-make)
  "A alist of rule to determine the name of a wconf from a buffer.
A rule is a function taking a BUFFER in parameter and returning a name of a
window configuration or nil if it does not match.
Each rule have a priority which determines the order in which rules are
evaluated.  The first rule that returns a non-nil value is the one that will be
applied.")
(defvar mugu-wconf-history (list)
  "An historic of visited vconf.")

(defun mugu-wconf-current ()
  "Return the name of the current wconf."
  (-first-item mugu-wconf-history))

(defun mugu-wconf-save (wconf-name)
  "Save current windows configuration under WCONF-NAME."
  (when wconf-name
    (ht-set mugu-wconf-map wconf-name (current-window-configuration))))

(defun mugu-wconf-make-new ()
  "Clear the current windows configuration."
  (select-window (frame-first-window))
  (mugu-window-delete-all-windows))

(defun mugu-wconf-load (wconf-name)
  "Restore window configuration saved under WCONF-NAME."
  (when wconf-name
    (message "before load history %s" mugu-wconf-history)
    (push wconf-name mugu-wconf-history)
    (setq mugu-wconf-history (-uniq mugu-wconf-history))
    (message "after load history %s" mugu-wconf-history)
    (if (ht-contains? mugu-wconf-map wconf-name)
        (set-window-configuration (ht-get mugu-wconf-map wconf-name))
      (mugu-wconf-make-new))))

(defun mugu-wconf-switch (wconf-name)
  "Save current wconf and restore the one under WCONF-NAME."
  (mugu-wconf-save (mugu-wconf-current))
  (mugu-wconf-load wconf-name))

(defun mugu-wconf-add-rule (priority buffer-rule-function)
  "Register a new wconf rule.
The correct wconf will be choosen according
See `mugu-wconf-rules' for details about format of PRIORITY and BUFFER-RULE-FUNCTION."
  (asoc-put! mugu-wconf-rules priority buffer-rule-function))

(defun mugu-wconf-of-buffer (buffer)
  "Return the WCONF associated to BUFFER according to `mugu-wconf-rules'."
  (-first-item
   (-non-nil
    (--map (funcall it buffer) (asoc-values (asoc-sort-keys mugu-wconf-rules '>))))))

(defun mugu-wconf-update-wconf (buffer)
  "Reload the wconf associated to the BUFFER according to `mugu-wconf-rules'."
  (unless (mugu-window-side-managed-p (get-buffer-window buffer))
    (let ((old-wconf-name (mugu-wconf-current))
          (new-wconf-name (mugu-wconf-of-buffer buffer)))
      (unless (eq old-wconf-name new-wconf-name)
        (mugu-wconf-switch new-wconf-name)))))

(define-minor-mode mugu-wconf-mode
  "A minor mode to automatically manage windows configuration according to
simple rules.
Use `mugu-wconf-add-rule' to define more rule."
  :global t
  (cond
   (mugu-wconf-mode
    (add-hook 'mugu-buffer-before-switch-functions #'mugu-wconf-update-wconf))
   (t
    (remove-hook 'mugu-buffer-before-switch-functions #'mugu-wconf-update-wconf))))

(provide 'mugu-wconf)
;;; mugu-wconf ends here
