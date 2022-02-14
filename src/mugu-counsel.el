;;; mugu-counsel --- Summary -*- lexical-binding: t -*-
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

(defun mugu-counsel--thing-at-point ()
  "Retrieve the thing at point taking into account visual state."
  (if (evil-visual-state-p)
      (let* ((visual-range (evil-visual-range))
             (beg (-first-item visual-range))
             (end (-second-item visual-range)))
        (evil-change-to-previous-state)
        (buffer-substring-no-properties beg end))
    (thing-at-point 'symbol t)))

;; * Interactive grep commands

(defun mugu-counsel-super-star ()
  "A ivy swiper variant of vim famous super star."
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-grep-or-swiper (mugu-counsel--thing-at-point))))

(defun mugu-counsel-hyper-star ()
  "A ivy swiper variant of vim famous super star.  Recursive!!"
  (interactive)
  (let ((ivy-hooks-alist '((t . mugu-ivy-active-menu))))
    (counsel-rg (mugu-counsel--thing-at-point))))

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

(defun mugu-counsel-generate-descbinds (initial-input)
  "Build a lambda describing bindings like `counsel-descbinds'.
INITIAL-INPUT will be forwarded to the built method which enable to tailor the
method to situation."
  (lambda ()
    (interactive)
    (ivy-read "Bindings: " (counsel--descbinds-cands nil (current-buffer))
              :initial-input initial-input
              :action #'counsel-descbinds-action-describe
              :history 'counsel-descbinds-history
              :caller 'counsel-descbinds)))

(defun mugu-counsel-set-config ()
  "."
  (add-hook 'counsel-grep-post-action-hook #'recenter))

;; * End
(provide 'mugu-counsel)
;;; mugu-counsel ends here
