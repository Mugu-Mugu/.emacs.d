;;; mugu-project-vterm --- Summary
;; An integration betwen project and terminal packages
;;; Commentary:
;; -*- lexical-binding: t -*-

;;; Code:
(require 'vterm)
(require 'ivy)
(require 'general)
(require 'mugu-buffer)
(require 'mugu-vterm)
(require 'mugu-project)
(require 'mugu-misc)

(defun mugu-pvterm-list-project-vterm (&optional project)
  "List vterm owned by PROJECT or current one if not specified."
  (if (or project (mugu-project-current-root))
      (mugu-pvterm--list-project-vterm (or project (mugu-project-current-root)))
    (mugu-pvterm--list-vterm-without-project)))

(defun mugu-pvterm--list-project-vterm (project)
  "List vterm owned by PROJECT."
  (--filter (and (mugu-vterm-buffer-vterm-p it)
                 (equal (mugu-project-name-of-buffer it) (mugu-project-name project)))
            (buffer-list)))

(defun mugu-pvterm--list-vterm-without-project ()
  "Return a list of vterm without project."
  (--filter (and (mugu-vterm-buffer-vterm-p it)
                 (not (mugu-project-of-buffer it)))
            (buffer-list))
  )

(defun mugu-pvterm-list-all-vterm-project-first ()
  "Return a list of all vterm opened with those from the current project first."
  (-uniq (-concat (mugu-pvterm-list-project-vterm (mugu-project-current-root))
                  (mugu-vterm-list-buffer-by-mru))))

(defun mugu-pvterm-after-creation (&optional project term-name commands)
  "Run action on recently created vterm.
Set its project to PROJECT or current project.
Rename it to TERM-NAME or PROJECT.
Then execute COMMANDS if any."
  (let* ((project-name (or (mugu-project-name project) (mugu-project-current-name)))
         (term-name (or term-name project-name)))
    (mugu-project-assign-buffer (current-buffer) (mugu-project-by-name project-name))
    (mugu-vterm-rename (current-buffer) term-name)
    (when commands
      (vterm-send-string commands t)
      (vterm-send-return))))

(defun mugu-pvterm-create-or-switch (project)
  "Create or switch to a vterm for PROJECT.
If PROJECT is not defined, `mugu-project-current-root' is used instead."
  (interactive (list (mugu-project-current-root)))
  (let ((mugu-vterm-list-buffer-function #'mugu-pvterm-list-project-vterm))
    (mugu-vterm-switch)))

(defun mugu-pvterm-create (&optional project term-name commands)
  "Create vterm for PROJECT named TERM-NAME.
If PROJECT is not defined, current-project is used instead.
Run COMMANDS upon creation if defined."
  (interactive)
  (with-temp-hook
    mugu-vterm-after-vterm-creation-hook
    (apply-partially #'mugu-pvterm-after-creation project term-name commands)
    (mugu-vterm-create)))

(defun mugu-pvterm-toggle ()
  "Switch to a vterm buffer or hide one if already displayed."
  (interactive)
  (let ((mugu-vterm-list-buffer-function #'mugu-pvterm-list-project-vterm))
    (with-temp-hook
      mugu-vterm-after-vterm-creation-hook
      #'mugu-pvterm-after-creation
      (mugu-vterm-toggle))))

(defun mugu-pvterm-activate ()
  "Activate the integration between project and vterm."
  (setq mugu-vterm-list-buffer-function #'mugu-pvterm-list-all-vterm-project-first)
  (general-define-key :states 'motion
                      [remap mugu-vterm-toggle] #'mugu-pvterm-toggle)
  (mugu-menu-add-entries 'mugu-project-menu
                         '("tt" mugu-pvterm-create-or-switch "create or switch to term" :column "Terminal")
                         '("tc" mugu-pvterm-create "create a term")))


(provide 'mugu-project-vterm)
;;; mugu-project-vterm ends here
