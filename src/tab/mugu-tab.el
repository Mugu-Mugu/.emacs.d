;;; mugu-tab --- Wrapper around tab-bar-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Wrapper around tab-bar-mode bringing vim like tabs to Emacs.  Unlike
;;; conventional tab in other editors, those tabs actually holds a window
;;; configuration rather than just a file.
;;; This wrapper adds some missing accessors in the original file, provide a
;;; coherent configuration and extension to perform automatic tab switch
;;; according to arbitrary rules


(require 'dash)
(require 'tab-bar)
(require 'asoc)
(require 'general)
(require 'cl-lib)

;;; Code:
;;; Variables
(defvar-local mugu-tab-pinned-tab-name nil "Tab name of tab owning the buffer.")
(defvar mugu-tab-local-variables-list (list)
  "A list of variables symbols that are local to a tab.")
(defvar mugu-tab-attribution-functions (list #'mugu-tab-attribution-rule-pinned)
  "A list of functions to determine the tab of a buffer.
Functions in the list will be evaluated until one returns a non-nil result.
Each function should take a BUFFER-OR-NAME as argument and returns a string if a
tab should own the buffer or nil if the rule doesn't cover it.")
(defvar mugu-tab-mode nil "To silence warnings.")
(defvar mugu-tab-after-switch-hook (list)
  "Hook run just after a tab switch has occurred.")
(defvar mugu-tab-before-switch-hook (list)
  "Hook run just before a tab switch will occur.")
(defvar mugu-tab-after-creation-hook (list)
  "Hook run just before a tab switch will occur.")

;;; Variables management
(defun mugu-tab-make-variable-local (var-symbol)
  "Make VAR-SYMBOL tab local."
  (when (symbolp var-symbol)
    (push var-symbol mugu-tab-local-variables-list)))

(defun mugu-tab--var-property-name (var-name)
  "Return a symbol for a local property for VAR-NAME."
  (intern (format "mugu-tab-local-var-%s-%s" (mugu-tab-current-tab-name) var-name)))

(defun mugu-tab--store-local-variables (&rest _)
  "."
  (-each mugu-tab-local-variables-list
    (lambda (var-symbol)
      (put var-symbol (mugu-tab--var-property-name var-symbol) (symbol-value var-symbol)))))

(defun mugu-tab--restore-local-variables (&rest _)
  "."
  (-each mugu-tab-local-variables-list
    (lambda (var-symbol)
      (set var-symbol
           (get var-symbol (mugu-tab--var-property-name var-symbol))))))

;;; Macros
(defmacro save-current-tab (&rest body)
  "Evaluate BODY and then restore current tab and return BODY value."
  `(let ((current-tab-name (mugu-tab-current-tab-name)))
     (prog1
         (progn ,@body)
       (mugu-tab-switch current-tab-name))))

;;; Accessors
(defun mugu-tab-current-tab ()
  "Return the current active tab."
  (assq 'current-tab (tab-bar-tabs)))

(defun mugu-tab-get-tab-with-name (name)
  "Return a tab with NAME."
  (--find (equal (mugu-tab-tab-name it) name) (tab-bar-tabs)))

(defun mugu-tab-tab-name (tab)
  "Return the name of a TAB."
  (cdr (assq 'name tab)))

(defun mugu-tab-current-tab-name ()
  "Return the name of the current tab."
  (mugu-tab-tab-name (mugu-tab-current-tab)))

;;; Actions
(defun mugu-tab-delete (&optional tab-name)
  "Delete tab with TAB-NAME or current one if nil."
  (interactive)
  (let ((tab-name (or tab-name (mugu-tab-current-tab-name))))
    (tab-bar-close-tab-by-name tab-name)))

(defun mugu-tab-try-delete (tab-name)
  "Delete tab with TAB-NAME if it exists."
  (when (mugu-tab-get-tab-with-name tab-name)
    (mugu-tab-delete tab-name)))

(defun mugu-tab-clone ()
  "Duplicate current window configuration in a new tab."
  (interactive)
  (let ((current-wconf (current-window-configuration)))
    (mugu-tab-new)
    (set-window-configuration current-wconf)))

(defun mugu-tab-pin-buffer (&optional buffer tab-name)
  "Pin BUFFER to tab with TAB-NAME."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (tab-name (or tab-name (mugu-tab-current-tab-name))))
    (with-current-buffer buffer
      (setq mugu-tab-pinned-tab-name tab-name))))

(defun mugu-tab-unpin-buffer (&optional buffer)
  "Unpin BUFFER from tab."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq mugu-tab-pinned-tab-name nil)))

(defun mugu-tab-new (tab-name)
  "Create a new tab with TAB-NAME."
  (tab-new)
  (tab-rename tab-name)
  (run-hooks 'mugu-tab-after-creation-hook))

(defalias 'mugu-tab-rename 'tab-bar-rename-tab)

;;; Selection
(defun mugu-tab-switch-or-create (tab-name)
  "Switch to tab with TAB-NAME creating it if needed."
  (unless (eq tab-name (mugu-tab-current-tab-name))
    (run-hooks 'mugu-tab-before-switch-hook)
    (mugu-tab--store-local-variables)
    (if (mugu-tab-get-tab-with-name tab-name)
        (progn
          (tab-bar-switch-to-tab tab-name)
          (mugu-tab--restore-local-variables))
      (mugu-tab-new tab-name))
    (run-hooks 'mugu-tab-after-switch-hook)))

(defalias 'mugu-tab-switch #'tab-bar-switch-to-tab)
(defalias 'mugu-tab-switch-to-next #'tab-bar-switch-to-next-tab)
(defalias 'mugu-tab-switch-to-previous #'tab-bar-switch-to-prev-tab)

;;; tab attribution management
(defun mugu-tab-attribution-rule-add (tab-attribution-function)
  "Add TAB-ATTRIBUTION-FUNCTION to `mugu-tab-attribution-functions'."
  (add-to-list 'mugu-tab-attribution-functions tab-attribution-function 'append))

(defun mugu-tab-attribution-rule-remove (tab-attribution-function)
  "Remove TAB-ATTRIBUTION-FUNCTION from `mugu-tab-attribution-functions'."
  (setq mugu-tab-attribution-functions (delete tab-attribution-function mugu-tab-attribution-functions)))

(defun mugu-tab-attribution-evaluate (buffer)
  "Determine the tab-name that should own BUFFER according to recorded rules."
  (-first 'identity (--map (funcall it buffer) mugu-tab-attribution-functions)))

(defun mugu-tab-attribution-rule-pinned (buffer)
  "A tab attribution rule based upon pinned status of the BUFFER."
  (buffer-local-value 'mugu-tab-pinned-tab-name buffer))

;;; Display functions
(defun mugu-tab-display-buffer (buffer alist)
  "Display-buffer action switching tab automatically if required.
TAB attribution of BUFFER is determined by `mugu-tab-attribution-functions'.
ALIST is not used but will be forwarded to `display-buffer' functions."
  (when mugu-tab-mode
    (let* ((tab-name (mugu-tab-attribution-evaluate buffer))
           (alist (asoc-merge alist `((tab-name . ,tab-name)))))
      (when (and tab-name (not (equal tab-name (mugu-tab-current-tab-name))))
        (mugu-tab-switch-or-create tab-name))))
  nil)

(defun mugu-tab-change-tab (buffer &rest _args)
  "Switch tab before switching to BUFFER.
This is required because for some reason, even with
`switch-to-buffer-obey-display-actions', the `switch-to-buffer' still don't
listen to rules in `display-buffer-base-action' because it calls
`pop-to-buffer-same-window'.
Therefore the only way to fix this is to advice before it is called."
  (mugu-tab-display-buffer (get-buffer-create buffer) (list)))

;;; For display-buffer
(defun mugu-tab-tabify-display-buffer-alist-rules (buffer-pattern-or-predicate)
  "Change value of `display-buffer-alist' to take tabs into account.
All rules matching BUFFER-PATTERN-OR-PREDICATE will be udpated."
  (setq display-buffer-alist
        (--map
         (-let* (((condition . actions) it)
                 ((display-functions . alist) actions)
                 (new-display-functions (if (and (equal buffer-pattern-or-predicate condition)
                                                 (not (-contains? display-functions #'mugu-tab-display-buffer)))
                                            (cons #'mugu-tab-display-buffer display-functions)
                                          display-functions)))
           (cons condition
                 (cons new-display-functions alist)))
         display-buffer-alist)))

;;; Mode functions
(defun mugu-tab--activate ()
  "Initialize the mode."
  (general-def
    mugu-tab-mode-map
    [remap winner-undo] #'tab-bar-history-back
    [remap winner-redo] #'tab-bar-history-forward)
  (push #'mugu-tab-display-buffer (car display-buffer-base-action))
  (advice-add 'tab-bar-select-tab :before #'mugu-tab--store-local-variables)
  (advice-add 'tab-bar-select-tab :after #'mugu-tab--restore-local-variables)
  (advice-add 'pop-to-buffer-same-window :before #'mugu-tab-change-tab)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (when (eq 1 (length (tab-bar-tabs)))
    (mugu-tab-rename "default")))

(defun mugu-tab--deactivate ()
  "Initialize the mode."
  (setf (car display-buffer-base-action) (delete #'mugu-tab-display-buffer (car display-buffer-base-action)))
  (advice-remove 'tab-bar-select-tab #'mugu-tab--store-local-variables)
  (advice-remove 'tab-bar-select-tab #'mugu-tab--restore-local-variables)
  (advice-remove 'pop-to-buffer-same-window #'mugu-tab-change-tab)
  (tab-bar-mode -1)
  (tab-bar-history-mode -1))

(define-minor-mode mugu-tab-mode
  "A minor mode to provide window configuration management in tabs.
Extends mode tab-bar' with a few automation rules."
  :global t
  :keymap (make-sparse-keymap)
  :group 'mugu
  (if mugu-tab-mode
      (mugu-tab--activate)
    (mugu-tab--deactivate)))

(provide 'mugu-tab)
;;; mugu-tab ends here
