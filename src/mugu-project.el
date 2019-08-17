;;; mugu-project --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
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

(defun mugu-project-switch-buffer ()
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
  (if buffer-file-truename
      (projectile-project-root)
    (or projectile-project-root (mugu-project-root-from-name mugu-project-current-name))))

(defun mugu-project-name ()
  "Return the name of the current project."
  (projectile-project-name (mugu-project-root)))

(defun mugu-project-cd ()
  "Change directory to a child dir of the project."
  (interactive)
  (mugu-counsel-fzf-dir (projectile-project-root))
  (mugu-space-main))

(defun mugu-project-buffer-project (&optional buffer)
  "Retrieve the project name of BUFFER.
If BUFFER is not given, use `current-buffer' instead."
  (let* ((buffer (or buffer (current-buffer)))
         (buffer-cached-root (buffer-local-value 'projectile-project-root buffer))
         (buffer-is-file (buffer-local-value 'buffer-file-truename buffer))
         (buffer-project (or buffer-cached-root (with-current-buffer buffer (mugu-project-root)))))
    (when (and buffer-is-file (not buffer-cached-root))
      (with-current-buffer buffer (setq-local projectile-project-root buffer-project)))
    buffer-project))

(defun mugu-project-set-buffer-project (project-name &optional buffer)
  "Define PROJECT-NAME for BUFFER.
If BUFFER is nil, it applies to `current-buffer' instead."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local projectile-project-root project-name)))

(defun mugu-project-buffer-in-project-p (buffer &optional project)
  "Predicate determning if BUFFER is in project PROJECT.
If PROJECT is nil, current project is used if any."
  (equal (projectile-project-name project) (mugu-project-buffer-project buffer)))

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
  ("v" projectile-vc "version control" :column "Other")
  ("g" counsel-git-grep  "gitgrep")
  ("rg" (counsel-rg "" projectile-project-root) "ripgrep")
  ("d" mugu-project-cd "cd" :color blue :column "Find")
  ("b" (mugu-project-switch-buffer) "buffer" :color blue)
  ("f" (mugu-counsel-fzf-file (mugu-project-root)) "open file alternative")
  ("q" nil "quit menu" :color blue :column nil))

 (defun mugu-project-switch-buffer-global ()
  "Switch to another buffer changing project and wconf if nessecary."
  (interactive)
  (let* ((old-project (mugu-project-buffer-project (current-buffer)))
         (new-buffer (save-window-excursion (call-interactively 'ivy-switch-buffer)
                                            (current-buffer)))
         (new-project (mugu-project-buffer-project new-buffer)))
    ;; (message "switching to buffer %s from old-project %s to new-project %s" new-buffer old-project new-project)
    (when (and new-project (projectile-project-p new-project))
      (projectile-switch-project-by-name new-project))
    (switch-to-buffer new-buffer)))

(defun mugu-project-save-wconf ()
  "Save current windows configuration."
  (when (mugu-project-name)
    (ht-set mugu-project-wconf-map (mugu-project-name) (current-window-configuration))))

(defun mugu-project-restore-wconf ()
  "Restore project windows configuration or clear it if new after switch."
  (if (ht-contains? mugu-project-wconf-map (mugu-project-name))
      (set-window-configuration (ht-get mugu-project-wconf-map (mugu-project-name)))
    (delete-other-windows)))

(defun mugu-project-after-switch ()
  "Actions after project switch: set current project and open a default buffer."
  (mugu-project-restore-wconf)
  ;; (message "switching to project %s" projectile-project-name)
  (setq mugu-project-current-name projectile-project-name)
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
