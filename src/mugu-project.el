;;; mugu-project --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'mugu-counsel)
(require 'mugu-directory)
(require 'mugu-menu)
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
              :action 'switch-to-buffer)))

(defun mugu-project-root ()
  "Return the project of `current-buffer'.
For non-file buffer, the behavious is special as they dont have a meaningful
`default-directory'."
  (let ((projectile-project-root-cache (ht-create)))
    (if buffer-file-truename
        (projectile-project-root)
      (or projectile-project-root (mugu-project-root-from-name mugu-project-current-name)))))

(defun mugu-project-name ()
  "Return the name of the current project."
  (projectile-project-name (mugu-project-root)))

(defun mugu-project-cd ()
  "Change directory to a child dir of the project."
  (interactive)
  (mugu-counsel-fzf-dir (projectile-project-root))
  (mugu-space-main))

(defun mugu-project-buffer-project (&optional buffer)
  "Retrieve the project root of BUFFER.
If BUFFER is not given, use `current-buffer' instead."
  (let* ((buffer (or buffer (current-buffer)))
         (buffer-cached-root (buffer-local-value 'projectile-project-root buffer))
         (buffer-is-file (buffer-local-value 'buffer-file-truename buffer)))
    (message "for buffer %s cached root is %s and is file is %s" buffer buffer-cached-root buffer-is-file)
    (or buffer-cached-root
        (and buffer-is-file (with-current-buffer buffer
                              (setq-local projectile-project-root (mugu-project-root)))))))

(defun mugu-project-set-buffer-project (project-name &optional buffer)
  "Define PROJECT-NAME for BUFFER.
If BUFFER is nil, it applies to `current-buffer' instead."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local projectile-project-root (mugu-project-root-from-name project-name))))

(defun mugu-project-vc ()
  "Call VC backend for current project."
  (interactive)
  (noflet ((projectile-project-root (&optional _dir) (mugu-project-root-from-name mugu-project-current-name)))
    (projectile-vc)))

(defun mugu-project-buffer-in-project-p (buffer &optional project-name)
  "Predicate determning if BUFFER is in project named PROJECT-NAME.
If PROJECT-NAME is nil, current project name is used if any.
Name is used instead of project because of ~ and other shnenanigans."
  (equal (or project-name mugu-project-current-name)
         (projectile-project-name (mugu-project-buffer-project buffer))))

(defmenu mugu-project-menu
  (:color blue :hint nil :body-pre (unless (mugu-project-root)
                                     (call-interactively 'projectile-switch-project)))
  "
                              -- PROJECT MENU --

  -> Project Root : %s(mugu-project-root)
  -> Current Dir  : %s(mugu-directory-pwd)
"
  ("s" projectile-switch-project "switch project" :color red :column "1-Management")
  ("a" projectile-add-known-project "register new project" :color red)
  ("u" projectile-remove-known-project "unregister project" :color red)
  ("v" mugu-project-vc "version control" :column "Other")
  ("g" counsel-git-grep  "gitgrep")
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

(defun mugu-project-pop-to-buffer (buffer &rest args)
  "Switch to BUFFER using `pop-to-buffer' switching project if required.
ARGS are passed as is to `pop-to-buffer'"
  (mugu-project--set-project-of-buffer buffer)
  (apply #'pop-to-buffer buffer args))

(defun mugu-project-save-wconf ()
  "Save current windows configuration."
  (when (mugu-project-name)
    (ht-set mugu-project-wconf-map (mugu-project-name) (current-window-configuration))))

(defun mugu-project-restore-wconf (project-name)
  "Restore project windows configuration of project PROJECT-NAME.
If there was no saved for PROJECT-NAME, clear all windows."
  (if (ht-contains? mugu-project-wconf-map project-name)
      (set-window-configuration (ht-get mugu-project-wconf-map project-name))
    (delete-other-windows)))

(defun mugu-project-after-switch ()
  "Actions after project switch: set current project and open a default buffer."
  (setq mugu-project-current-name projectile-project-name)
  (mugu-project-restore-wconf projectile-project-name)
  (unless (projectile-project-buffer-p (current-buffer) default-directory)
    (switch-to-buffer "*Messages*")))

(defun mugu-project-activate ()
  "Configure perspective and projectile in a coherent feature."
  (add-hook 'projectile-before-switch-project-hook 'mugu-project-save-wconf)
  (add-hook 'projectile-find-dir-hook 'mugu-project-save-wconf)
  (setq projectile-switch-project-action 'mugu-project-after-switch))

(defun mugu-project-hyper-star ()
  "Search thing at point in project."
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-rg (thing-at-point 'symbol t) (projectile-project-root))))

(provide 'mugu-project)
;;; mugu-project ends here
