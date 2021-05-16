;;; mugu-project --- Summary -*- lexical-binding: t -*-
;; This is a complete wraper of projectile
;;; Commentary:

;;; Code:

;; * begin:
(require 'mugu-counsel)
(require 'mugu-directory)
(require 'mugu-menu)
(require 'mugu-window)
(require 'mugu-space)
(require 'projectile)
(require 'dash)
(require 'ht)
(require 'f)


;; * Variable
(defvar-local mugu-project-pinned-root
  nil
  "Pinned project root of a buffer.  Will bypass all other rules.")
(defvar mugu-project-root-fallback-functions
  (list #'mugu-project-root-visible
        #'mugu-project-root-last-visited)
  "A list of functions to determine project root of current buffer.
Each function will be called until one return a non-nil result.
No argument is passed on call invocation.")

;; Pin method
(defun mugu-project-pin-buffer (&optional buffer project-root)
  "Pin BUFFER to the project defined by PROJECT-ROOT."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (project-root (or project-root (mugu-project-current-root))))
    (with-current-buffer buffer
      (setq mugu-project-pinned-root project-root))))

(defun mugu-project-unpin-buffer (&optional buffer)
  "Unpin BUFFER from any project."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (setq mugu-project-pinned-root nil))))

;; Root determination methods
(defalias 'mugu-project-current-project-name #'projectile-project-name)

(defun mugu-project--projectile-root (original-root-function &optional directory)
  "Replacement for `projectile-project-root'.
It will return the project root applicable for current buffer and will backup on
fallback methods defined in `mugu-project-root-fallback-functions' which may use
more information such as current window configuration/history/etc...
ORIGINAL-ROOT-FUNCTION should be `projectile-project-root'.
DIRECTORY is not supported and will raise an error if defined."
  (if (and directory (not (f-same? directory default-directory)))
      (funcall original-root-function directory)
    (or mugu-project-pinned-root
        (and buffer-file-truename (funcall original-root-function))
        (-first-item (-map 'funcall mugu-project-root-fallback-functions)))))

(defun mugu-project-root-visible ()
  "Return the project of visible roots."
  (->> (window-list)
       (--map (->> it
                   (window-buffer)
                   (mugu-project-root-of-buffer)))
       (-non-nil)
       (-first-item)))

(defun mugu-project-root-last-visited ()
  "Return the project of visible roots."
  (mugu-project-root-of-buffer (-first #'mugu-project-root-of-buffer (buffer-list))))

(defalias 'mugu-project-root #'projectile-project-root)
(defalias 'mugu-project-current-root #'projectile-project-root)

;; Project buffers methods
(defun mugu-project-root-of-buffer (buffer)
  "Return the project root of BUFFER."
  (let ((mugu-project-root-fallback-functions))
    (and buffer
         (with-current-buffer buffer (mugu-project-root)))))

(defun mugu-project-buffer-in-project-p (buffer project-root)
  "Predicate indicating if BUFFER is included in project with PROJECT-ROOT."
  (f-same? (or (mugu-project-root-of-buffer buffer) "/")
           project-root))

(defun mugu-project-buffers (project-root)
  "Return a list of buffer in project with PROJECT-ROOT."
  (-filter
   (lambda (buffer) (mugu-project-buffer-in-project-p buffer project-root))
   (buffer-list)))

(defun mugu-project-switch-buffer-in-project ()
  "Interactively switch to a buffer in current project."
  (ivy-read (format "Select a buffer in project %s" (mugu-project-name (mugu-project-root)))
            (-map 'buffer-name
                  (-remove-first 'identity
                                 (mugu-project-buffers (mugu-project-root))))
            :action 'switch-to-buffer))

;; accessor
(defun mugu-project-name (project)
  "Return the name of the PROJECT."
  (let ((projectile-project-name))
    (projectile-project-name project)))

(defun mugu-project-by-name (name)
  "Return the project matching exactly NAME."
  (let ((projectile-project-name))
    (--first
     (equal (projectile-project-name it) name)
     projectile-known-projects)))

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
  ("rg" (counsel-rg "" (mugu-project-current-root)) "ripgrep")
  ("d" mugu-project-cd "cd" :color blue :column "Find")
  ("b" (mugu-project-switch-buffer-in-project) "buffer" :color blue)
  ("f" mugu-project-find-file "open file alternative")
  ("q" nil "quit menu" :color blue :column nil))

(defun mugu-project-hyper-star ()
  "Search thing at point in project."
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-rg (thing-at-point 'symbol t) (mugu-project-current-root))))

;; * Mode related functions
(defun mugu-project--activate ()
  "."
  (advice-add #'projectile-project-root :around #'mugu-project--projectile-root)
  (projectile-mode 1))

(defun mugu-project--deactivate ()
  "."
  (advice-remove #'projectile-project-root #'mugu-project--projectile-root)
  (projectile-mode -1))

(define-minor-mode mugu-project-mode
  "A minor mode to activate automatic tab management based on buffer project."
  nil
  :global t
  :group 'mugu
  (if mugu-project-mode
      (mugu-project--activate)
    (mugu-project--deactivate)))

(provide 'mugu-project)
;;; mugu-project ends here
