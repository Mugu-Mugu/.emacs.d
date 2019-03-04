;;; mugu-project-utils --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'mugu-directory-fix)
(require 'mugu-menu)
(require 'persp-projectile)
(require 'projectile)
(require 'perspective)
(require 'dash)

;; * Code
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
  (:color blue :hint nil :body-pre (unless (projectile-project-root) (call-interactively 'projectile-persp-switch-project)))
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

(defun mugu-project-activate ()
  "Configure perspective and projectile in a coherent feature."
  )

(provide 'mugu-project-utils)
;;; mugu-project-utils ends here
