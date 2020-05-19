;;; mugu-project --- Summary -*- lexical-binding: t -*-
;; This is a complete wraper of projectile
;;; Commentary:

;;; Code:

;; * begin:
(require 'mugu-counsel)
(require 'mugu-directory)
(require 'mugu-menu)
(require 'mugu-buffer)
(require 'mugu-window)
(require 'mugu-space)
(require 'projectile)
(require 'dash)
(require 'ht)

;; * Code
(defun mugu-project-buffer-in-project-p (buffer project)
  "Predicate indicating if BUFFER is in PROJECT."
  (projectile-project-buffer-p buffer project))

(defun mugu-project-buffer-selector (project)
  "Return a predicate to identify buffer in project with PROJECT."
  (let ((default-directory project))
    (lambda (buffer)
      (projectile-project-buffer-p buffer project))))

(defun mugu-project-buffers (project)
  "Return a list of buffer in PROJECT."
  (-filter (mugu-project-buffer-selector (file-truename project)) (buffer-list)))

(defun mugu-project-switch-buffer-in-project ()
  "Interactively switch to a buffer in current project."
  (ivy-read (format "Select a buffer in project %s" (mugu-project-current-name))
            (-map 'buffer-name
                  (-remove-first 'identity
                                 (mugu-project-buffers (mugu-project-current-root))))
            :action 'switch-to-buffer))

(defun mugu-project-name (project)
  "Return the name of the PROJECT."
  (projectile-project-name project))

(defun mugu-project-name-of-buffer (buffer)
  "Return the project name of BUFFER.
If there is none, return the one of the  last project visited."
  (let ((buffer-project (mugu-project-of-buffer buffer)))
    (when buffer-project (projectile-project-name buffer-project))))

(defun mugu-project-of-buffer (buffer)
  "Return the project of BUFFER or nil if it does not exists."
  (when  buffer
    (with-current-buffer buffer
      (or
       projectile-project-root
       (and buffer-file-truename (projectile-project-root))))))

(defun mugu-project-current-root ()
  "Return the current project."
  (mugu-project-of-buffer (-first #'mugu-project-of-buffer (buffer-list))))

(defun mugu-project-current-name ()
  "Return the current project name."
  (projectile-project-name (mugu-project-current-root)))

(defun mugu-project-assign-buffer (buffer project)
  "Assign BUFFER to PROJECT."
  (with-current-buffer buffer (setq projectile-project-root project)))

(defun mugu-project-generate-scratch-buffer (project)
  "Create a scratch buffer for the PROJECT and return it."
  (let* ((project-name (projectile-project-name project))
         (scratch-buffer (get-buffer-create (format "scratch (%s)" project-name))))
    (with-current-buffer scratch-buffer
      (mugu-project-assign-buffer scratch-buffer project))
    scratch-buffer))

(defun mugu-project-cd ()
  "Change directory to a child dir of the project."
  (interactive)
  (mugu-counsel-fzf-dir (mugu-project-current-root))
  (mugu-space-main))

(defun mugu-project-vc ()
  "Call VC backend for current project."
  (interactive)
  (projectile-vc (mugu-project-current-root)))

(defun mugu-project-find-file (&optional root)
  "Find files in project at ROOT or current."
  (interactive)
  (mugu-counsel-fzf-file (or root (mugu-project-current-root))))

(defmenu mugu-project-menu
  (:color blue :hint nil :body-pre (unless (mugu-project-current-root)
                                     (call-interactively 'projectile-switch-project)))
  "
                              -- PROJECT MENU --

  -> Project Root : %s(mugu-project-current-root)
  -> Current Dir  : %s(mugu-directory-pwd)
"
  ("s" projectile-switch-project "switch project" :color red :column "1-Management")
  ("a" projectile-add-known-project "register new project" :color red)
  ("u" projectile-remove-known-project "unregister project" :color red)
  ("v" mugu-project-vc "version control" :column "Other")
  ("g" (let ((default-directory (mugu-project-current-root))) (counsel-git-grep)) "gitgrep")
  ("rg" (counsel-rg "" projectile-project-root) "ripgrep")
  ("d" mugu-project-cd "cd" :color blue :column "Find")
  ("b" (mugu-project-switch-buffer-in-project) "buffer" :color blue)
  ("f" mugu-project-find-file "open file alternative")
  ("q" nil "quit menu" :color blue :column nil))

(defun mugu-project-load-buffer-after-switch ()
  "Switch to NEW-PROJECT changing wconf and projectile internal."
  ;; (message "project buffers: %s" (mugu-project-buffers (projectile-project-name default-directory)))
  (switch-to-buffer (or (-second-item (mugu-project-buffers default-directory))
                        (mugu-project-generate-scratch-buffer default-directory))))


(defun mugu-project-activate ()
  "Configure perspective and projectile in a coherent feature."
  (setq projectile-switch-project-action #'mugu-project-load-buffer-after-switch))

(defun mugu-project-hyper-star ()
  "Search thing at point in project."
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-rg (thing-at-point 'symbol t) (mugu-project-current-root))))

(provide 'mugu-project)
;;; mugu-project ends here
