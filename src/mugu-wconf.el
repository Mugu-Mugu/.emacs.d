;;; mugu-wconf --- Provides automatic window configuration management -*- lexical-binding: t -*-
;;; Commentary:
;;  TODO add a wrapper to add buffer-alist so customization can be managed
;;  TODO or add a function that automatically modifiy this variable to provide the feature
;;  TODO remove the configuration on mode exit
;;  TODO add customizable pattern to exclude some buffer from it
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
;; (defvar mugu-wconf-mode nil
;;   "The mode variable for wconf.")

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
    ;(message "before load history %s" mugu-wconf-history)
    (setq mugu-wconf-history (cons wconf-name (-remove-item wconf-name mugu-wconf-history)))
    ;; (message "after load history %s" mugu-wconf-history)
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

(defun mugu-wconf-ignored-buffer-p (buffer)
  "Predicate determing if BUFFER should be ignored by automatic wconf.
It is not possible to determine if the new buffer would be displayed in a side
window.  Since this determines wether or not a wconf switch is needed we need to
make a fake switch."
  (let ((display-buffer-overriding-action))
    (save-window-excursion
      (display-buffer buffer)
      (or (mugu-window-side-managed-p (get-buffer-window buffer))
          (equal (buffer-name buffer) " *LV*")))))

(defun mugu-wconf-update (buffer _alist)
  "Fake `display-buffer' action.
Change the window configuration according to `mugu-wconf-rules'.
BUFFER and ALIST are as in `display-buffer'."
  (unless (mugu-wconf-ignored-buffer-p buffer)
    (let ((old-wconf-name (mugu-wconf-current))
          (new-wconf-name (mugu-wconf-of-buffer buffer)))
      (when (and new-wconf-name
                 (not (eq old-wconf-name new-wconf-name)))
        (mugu-wconf-switch new-wconf-name))))
  nil)

(define-minor-mode mugu-wconf-mode
  "A minor mode to automatically manage windows configuration according to
simple rules.
Use `mugu-wconf-add-rule' to define more rule."
  :global t
  (cond
   (mugu-wconf-mode
    ;; HACK TODO this is not the correct way (in therory)
    (setq display-buffer-overriding-action  '(mugu-wconf-update)))))

(provide 'mugu-wconf)
;;; mugu-wconf ends here
