;;; mugu-project --- Summary
;; tbc
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
(defvar mugu-project-wconf-map (ht-create) "Gather windows configuration for each project.")
(defvar mugu-project-current-name "emacs.d" "Tracks current project name.")

(defun mugu-project-root-from-name (project-name)
  "Find the project root from PROJECT-NAME.
Note that this may fails when several project have the same name."
  (-first-item
   (--filter (equal project-name (projectile-project-name it)) projectile-known-projects)))

(defun mugu-project-switch-buffer-in-project ()
  "Interactively switch to a buffer in current project."
  (let ((buffers-in-project (-map 'buffer-name
                                  (-filter 'mugu-project-buffer-in-project-p
                                           (-remove-first 'identity (buffer-list))))))
    (ivy-read (format "Select a buffer in project %s" (projectile-project-name))
              buffers-in-project
              :action 'mugu-buffer-switch)))

(defun mugu-project-root ()
  "Return the project of `current-buffer'.
If buffer has no project, return current active one instead."
  (or (mugu-project-buffer-project) (mugu-project-root-from-name mugu-project-current-name)))

(defun mugu-project-name ()
  "Return the name of the current project."
  (projectile-project-name (mugu-project-root)))

(defun mugu-project-cd ()
  "Change directory to a child dir of the project."
  (interactive)
  (mugu-counsel-fzf-dir (mugu-project-root))
  (mugu-space-main))

(defun mugu-project-buffer-project (&optional buffer)
  "Retrieve the project root of BUFFER.
If BUFFER is not given, use `current-buffer' instead."
  (with-current-buffer (or buffer (current-buffer))
    (if (or projectile-project-root (not buffer-file-truename))
        projectile-project-root
      (setq projectile-project-root (projectile-project-root)))))

(defun mugu-project-set-buffer-project (project-name &optional buffer)
  "Define PROJECT-NAME for BUFFER.
If BUFFER is nil, it applies to `current-buffer' instead."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local projectile-project-root (mugu-project-root-from-name project-name))))

(defun mugu-project-vc ()
  "Call VC backend for current project."
  (interactive)
  (projectile-vc (mugu-project-root)))

(defun mugu-project-buffer-in-project-p (buffer &optional project-name)
  "Predicate determning if BUFFER is in project named PROJECT-NAME.
If PROJECT-NAME is nil, current project name is used if any.
Name is used instead of project because of ~ and other shnenanigans."
  (let ((buffer-project (mugu-project-buffer-project buffer)))
    (equal (or project-name mugu-project-current-name)
           (and buffer-project (projectile-project-name buffer-project)))))

(defmenu mugu-project-menu
  (:color blue :hint nil :body-pre (unless (mugu-project-root)
                                     (call-interactively 'mugu-project-switch)))
  "
                              -- PROJECT MENU --

  -> Project Root : %s(mugu-project-root)
  -> Current Dir  : %s(mugu-directory-pwd)
"
  ("s" mugu-project-switch "switch project" :color red :column "1-Management")
  ("a" projectile-add-known-project "register new project" :color red)
  ("u" projectile-remove-known-project "unregister project" :color red)
  ("v" mugu-project-vc "version control" :column "Other")
  ("g" (let ((default-directory (mugu-project-root))) (counsel-git-grep)) "gitgrep")
  ("rg" (counsel-rg "" projectile-project-root) "ripgrep")
  ("d" mugu-project-cd "cd" :color blue :column "Find")
  ("b" (mugu-project-switch-buffer-in-project) "buffer" :color blue)
  ("f" (mugu-counsel-fzf-file (mugu-project-root)) "open file alternative")
  ("q" nil "quit menu" :color blue :column nil))

(defun mugu-project--set-project-of-buffer (buffer)
  "Change the project to the one owning BUFFER."
  (let* ((current-project (mugu-project-root-from-name mugu-project-current-name))
         (old-project (or (mugu-project-buffer-project (current-buffer)) current-project))
         (new-project (or (mugu-project-buffer-project buffer) current-project)))
    (when (not (equal (projectile-project-name old-project)
                      (projectile-project-name new-project)))
      (projectile-switch-project-by-name new-project))))

(defun mugu-project-switch-buffer (buffer)
  "Switch to BUFFER changing project if required."
  (interactive (list (save-window-excursion (call-interactively #'ivy-switch-buffer)
                                            (current-buffer))))
  (mugu-project--set-project-of-buffer buffer)
  (switch-to-buffer buffer))

(defun mugu-project--save-wconf ()
  "Save current windows configuration."
  (when (mugu-project-name)
    (ht-set mugu-project-wconf-map (mugu-project-name) (current-window-configuration))))

(defun mugu-project--restore-wconf (project-name)
  "Restore project windows configuration of project PROJECT-NAME.
If there was no saved for PROJECT-NAME, clear all windows."
  (if (ht-contains? mugu-project-wconf-map project-name)
      (set-window-configuration (ht-get mugu-project-wconf-map project-name))
    (select-window (frame-first-window))
    (mugu-window-delete-all-windows)))

(defun mugu-project--maybe-display-default-buffer (new-project)
  "Change `current-buffer' if it is not owned by NEW-PROJECT."
  (unless (and (mugu-project-buffer-project)
               (equal (projectile-project-name) mugu-project-current-name))
    (switch-to-buffer "*Messages*")))

(defun mugu-project-switch (new-project)
  "Switch to NEW-PROJECT changing wconf and projectile internal."
  (interactive (list (ivy-read "Select a project: " (projectile-relevant-known-projects))))
  (mugu-project--save-wconf)
  (setq mugu-project-current-name (projectile-project-name new-project))
  (projectile-switch-project-by-name new-project)
  (mugu-project--restore-wconf (projectile-project-name new-project))
  (mugu-project--maybe-display-default-buffer new-project))

(defun mugu-project--switch-maybe (new-buffer)
  "Switch project in NEW-BUFFER if it is different than current buffer."
  (let* ((current-project (mugu-project-root-from-name mugu-project-current-name))
         (old-project (or (mugu-project-buffer-project (current-buffer)) current-project))
         (new-project (mugu-project-buffer-project new-buffer))
         (old-project-name (projectile-project-name old-project))
         (new-project-name (if (projectile-project-p new-project)
                               (projectile-project-name new-project)
                             "-")))
    (when (and new-project
               (not (equal new-project-name old-project-name)))
      (mugu-project-switch new-project))))

(defun mugu-project-activate ()
  "Configure perspective and projectile in a coherent feature."
  (add-hook 'mugu-buffer-before-switch-functions 'mugu-project--switch-maybe)
  (setq projectile-switch-project-action 'ignore))

(defun mugu-project-hyper-star ()
  "Search thing at point in project."
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-rg (thing-at-point 'symbol t) (mugu-project-root))))

(provide 'mugu-project)
;;; mugu-project ends here
