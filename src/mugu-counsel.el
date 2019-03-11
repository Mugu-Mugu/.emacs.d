;;; mugu-counsel --- Summary
;; defines various various reader method
;; reimplement async with hackish but way faster method
;;; Commentary:

;;; Code:
;; * Requirement
(require 'ivy)
(require 'counsel)
(require 'noflet)
(require 'mugu-directory)

;; * Interactive find files commands
(defun mugu-counsel--fd-cmd (&optional only-f-or-d)
  "Generate a fd command string to query files.
If ONLY-F-OR-D is nil, both file and directory are queried (fast).
if ONLY-F-OR-D is equal to 'only-file then directories are not included.
if ONLY-F-OR-D is equal to 'only-dir then files are not included."
  (let ((f-or-d (pcase only-f-or-d
                  ('only-file "--type f")
                  ('only-dir "--type d")
                  (_ ""))))
    (format "fd --hidden --follow --exclude '.git' %s" f-or-d )))

(defun mugu-counsel--fzf (prompt &optional initial-directory only-f-or-d)
  "Find recursivly with `fzf' and fd with PROMPT.
INITIAL-DIRECTORY ONLY-F-OR-D"
  (let ((original-fzf-cmd (getenv "FZF_DEFAULT_COMMAND")))
    (counsel-require-program "fd")
    (setenv "FZF_DEFAULT_COMMAND" (mugu-counsel--fd-cmd only-f-or-d))
    (counsel-fzf "" initial-directory prompt)
    (setenv "FZF_DEFAULT_COMMAND" original-fzf-cmd)))

(defun mugu-counsel-fzf-file (&optional directory)
  "Find file recursively at DIRECTORY."
  (mugu-counsel--fzf "Find files recursively: " directory 'only-file))

(defun mugu-counsel-fzf-dir (&optional directory)
  "Find file recursively at DIRECTORY."
  (noflet ((find-file (filename &optional wildcards) (mugu-directory-find-file-or-cd (file-truename filename))))
    (mugu-counsel--fzf "Find dir recursively: " directory 'only-dir)))

(defun mugu-counsel-fzf-any (&optional directory)
  "Find file recursively at DIRECTORY."
  (noflet ((find-file (filename &optional wildcards) (mugu-directory-find-file-or-cd filename)))
    (mugu-counsel--fzf "Find recursively: " directory)))

(defun mugu-counsel-cd (&optional directory)
  "Cd to a new directory starting from DIRECTORY."
  (interactive)
  (mugu-directory-cd (read-directory-name "Change dir: " mugu-directory)))

;; * Interactive grep commands

(defun mugu-counsel-super-star ()
  "A ivy swiper variant of vim famous super star."
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-grep-or-swiper (thing-at-point 'symbol t))))

(defun mugu-counsel-hyper-star ()
  "A ivy swiper variant of vim famous super star.  Recursive!!"
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-rg (thing-at-point 'symbol t))))

(defun mugu-counsel-describe-custom ()
  "List all customizable variables."
  (interactive)
  (ivy-read "Describe variable: " obarray
            :predicate #'custom-variable-p
            :require-match t
            :history 'counsel-describe-symbol-history
            :keymap counsel-describe-map
            :preselect (ivy-thing-at-point)
            :sort t
            :action (lambda (x)
                      (funcall counsel-describe-variable-function (intern x)))
            :caller 'counsel-describe-variable))

(defun mugu-counsel-set-config ()
  "."
  (add-hook 'counsel-grep-post-action-hook #'recenter))

;; * End
(provide 'mugu-counsel)
;;; mugu-counsel ends here
