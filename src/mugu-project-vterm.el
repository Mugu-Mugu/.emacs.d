;;; mugu-project-vterm --- Summary
;; An integration betwen project and terminal packages
;;; Commentary:
;; -*- lexical-binding: t -*-

;;; Code:
(require 'mugu-project)
(require 'vterm)
(require 'ivy)

(defun mugu-pvterm-list-project-vterm (project-name)
  "List vterm owned by PROJECT-NAME."
  (--filter (and
             (mugu-project-buffer-in-project-p it project-name)
             (eq 'vterm-mode (buffer-local-value 'major-mode it)))
            (buffer-list)))

(defun mugu-pvterm-create-or-switch (&optional project-name)
  "Create or switch to a vterm for PROJECT-NAME.
If PROJECT-NAME is not defined, `mugu-project-name' is used instead."
  (interactive)
  (let* ((project-name (or project-name (mugu-project-name)))
         (existing-vterms (mugu-pvterm-list-project-vterm project-name)))
    (message "%s" existing-vterms)
    (pcase (length existing-vterms)
      (0 (mugu-pvterm-create project-name))
      (1 (mugu-project-switch-buffer (-first-item existing-vterms)))
      (_ (mugu-project-switch-buffer (get-buffer
                                      (ivy-read (format "Select a terminal in project %s" project-name)
                                                (-map #'buffer-name existing-vterms))))))))

(defun mugu-pvterm-create (&optional project-name term-name commands)
  "Create vterm for PROJECT-NAME named TERM-NAME.
If PROJECT-NAME is not defined, current-project is used instead.
Run COMMANDS upon creation if defined."
  (interactive)
  (let* ((project-name (or project-name (mugu-project-name)))
         (term-name (or term-name project-name)))
    (vterm (format "vterm (%s)" term-name))
    (mugu-project-set-buffer-project project-name)
    (when commands
      (vterm-send-string commands t)
      (vterm-send-return))))

(provide 'mugu-project-vterm)
;;; mugu-project-vterm ends here
