;;; mugu-project-utils --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'mugu-directory-fix)
(require 'mugu-menu)
(require 'projectile)
(require 'dash)
(require 'ht)

;; * Code
(defvar mugu-project-wconf-map (ht-create) "Gather windows configuration for each project.")

(defun mugu-project-switch-buffer ()
  "Interactively switch to a buffer in current project."
  (let ((buffers-in-project (-map 'buffer-name
                                  (-filter 'mugu-project-buffer-in-project-p
                                           (-remove-first 'identity (buffer-list))))))
    (ivy-read (format "Select a buffer in project %s" (projectile-project-name))
              buffers-in-project
              :action 'switch-to-buffer)))

(defun mugu-project-buffer-in-project-p (buffer &optional project)
  "Predicate determning if BUFFER is in project PROJECT.
If PROJECT is nil, current project is used if any."
  (let ((project (or project (projectile-project-root))))
    (and (buffer-file-name buffer) (f-ancestor-of? project (buffer-file-name buffer)))))

(defmenu mugu-project-menu
  (:color blue :hint nil :body-pre (unless (projectile-project-root) (call-interactively 'projectile-switch-project)))
  "
                              -- PROJECT MENU --

  -> Project Root : %s(projectile-project-root)
  -> Current Dir  : %s(mugu-directory-pwd)
"
  ("s" projectile-switch-project "switch project" :color red :column "1-Management")
  ("a" projectile-add-known-project "register new project" :color red)
  ("u" projectile-remove-known-project "unregister project" :color red)
  ("tr" projectile-regenerate-tags "regenerate tags" :column "3-Actions")
  ("v" projectile-vc "version control")
  ("d" (mugu-project-find-dir) "cd" :color red :column "2-Find")
  ("b" (mugu-project-switch-buffer) "buffer" :color blue)
  ("r" projectile-find-file "open file from all project")
  ("f" (find-file (mugu-counsel-find-file-recursive (projectile-project-root))) "open file alternative")
  ("tf" projectile-find-tag "lookup tag")
  ("g" (counsel-rg "" projectile-project-root) "grep")
  ("q" nil "quit menu" :color blue :column nil)
  ("SPC" mugu-menu-main-menu "return to main menu" :color blue))

(defun mugu-persp-switch-buffer ()
  "Switch to buffer interactivly with perspective change if needed."
  (interactive)
  ;; (call-interactively #'switch-to-buffer)
  (call-interactively #'ivy-switch-buffer)
  (let* ((project-name (projectile-project-name))
         (persp-name (persp-name (persp-curr)))
         (new-buffer (current-buffer)))
    (unless (eq persp-name project-name)
      (persp-switch project-name)
      (persp-add-buffer new-buffer)
      (switch-to-buffer new-buffer)
      (persp-switch-last)
      (persp-remove-buffer new-buffer)
      (persp-switch-last))))

(defun mugu-project-switch-buffer-global ()
  "Switch to another buffer changing project and wconf if nessecary."
  (interactive)
  (let* (target-project
         (new-buffer (save-window-excursion
                       (winner-mode -1)
                       (call-interactively 'ivy-switch-buffer)
                       (setq target-project (projectile-project-name))
                       (winner-mode 1)
                       (current-buffer))))
    (when target-project
      (projectile-switch-project-by-name target-project))
    (switch-to-buffer new-buffer)))

(defun mugu-project-save-wconf ()
  "Save current windows configuration."
  (when (projectile-project-name)
    (ht-set mugu-project-wconf-map (projectile-project-name) (current-window-configuration))))

(defun mugu-project-restore-wconf ()
  "Restore project windows configuration or clear it if new after switch."
  (if (ht-contains? mugu-project-wconf-map (projectile-project-name))
      (set-window-configuration (ht-get mugu-project-wconf-map (projectile-project-name)))
    (delete-other-windows)))

(defun mugu-project-after-switch ()
  "Prompt for file if current buffer is not part of the project."
  (mugu-project-restore-wconf)
  (unless (projectile-project-buffer-p (current-buffer) default-directory)
    (projectile-find-file)))

(defun mugu-project-activate ()
  "Configure perspective and projectile in a coherent feature."
  (add-hook 'projectile-before-switch-project-hook 'mugu-project-save-wconf)
  (setq projectile-switch-project-action 'mugu-project-after-switch))


(provide 'mugu-project-utils)
;;; mugu-project-utils ends here
