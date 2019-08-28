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

(defun mugu-pvterm-list-project-vterm (&optional project-name)
  "List vterm owned by PROJECT-NAME or current one if not specified."
  (--filter (and (mugu-vterm-buffer-vterm-p it)
                 (mugu-project-buffer-in-project-p it (or project-name (mugu-project-name))))
            (buffer-list)))

(defun mugu-pvterm-list-all-vterm-project-first ()
  "Return a list of all vterm opened with those from the current project first."
  (-uniq (-concat (mugu-pvterm-list-project-vterm (mugu-project-name))
                  (mugu-vterm-list-buffer-by-mru))))

(defun mugu-pvterm-after-creation (&optional project-name term-name commands)
  "Run action on recently created vterm.
Set its project to PROJECT-NAME or current project name.
Rename it to TERM-NAME or PROJECT-NAME.
Then execute COMMANDS if any."

  (let* ((project-name (or project-name (mugu-project-name)))
         (term-name (or term-name project-name)))
    (mugu-project-set-buffer-project project-name)
    (mugu-vterm-rename (current-buffer) term-name)
    (when commands
      (vterm-send-string commands t)
      (vterm-send-return))))

(defun mugu-pvterm-create-or-switch (project-name)
  "Create or switch to a vterm for PROJECT-NAME.
If PROJECT-NAME is not defined, `mugu-project-name' is used instead."
  (interactive (list (mugu-project-name)))
  (let ((mugu-vterm-list-buffer-function #'mugu-pvterm-list-project-vterm))
    (mugu-vterm-switch)))

(defun mugu-pvterm-create (&optional project-name term-name commands)
  "Create vterm for PROJECT-NAME named TERM-NAME.
If PROJECT-NAME is not defined, current-project is used instead.
Run COMMANDS upon creation if defined."
  (interactive)
  (with-temp-hook
    mugu-vterm-after-vterm-creation-hook
    (apply-partially #'mugu-pvterm-after-creation project-name term-name commands)
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
