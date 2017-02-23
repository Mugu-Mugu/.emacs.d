;;; Package --- Summary
;; All general purpose settings go there
;;; Commentary:

;;; Code:

;;; GUI conf
;; Don't show those horrible buttons
(tool-bar-mode -1)
;; garbage collection at 50MO
(setq gc-cons-threshold 50000000)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; disable auto-save and auto-backup
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)

;; Line numbers!
;; Disable vertical scrollbars in all frames.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; no fucking ping bell
(setq ring-bell-function 'ignore)

;;; FILE MGT conf
;; no lock file
(setq create-lockfiles nil)
;; UTF-8 everything!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Show me the new saved file if the contents change on disk when editing.
(global-auto-revert-mode 1)

;;; BUFFER MGT conf
;; The default of 16 is too low. Give me a 64-object mark ring.
;; Across all files, make it 128.
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)

;;; EDIT conf
;; break long lines at word boundaries
(visual-line-mode 1)
;; also tabs are evil
(setq-default indent-tabs-mode nil)
;; number columns in the status bar
(column-number-mode)
;; require a trailing newline
(setq require-final-newline t)
;; Only scroll one line when near the bottom of the screen, instead
;; of jumping the screen around.
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)
;; Display the current function name in the modeline.
(which-function-mode 0)

;;; STARTUP conf
;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)
;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;;; TO RELOCATE
;; from <https://github.com/bling/dotemacs/>
(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(global-hl-line-mode t)
(set-face-background 'hl-line "#3e4446")
;; Show parentheses
(show-paren-mode 1)
;; highlight entire expression when matching paren is not visible;
;; otherwise just highlight matching paren
(setq show-paren-style 'mixed)
(setq whitespace-style '(trailing))
(use-package whitespace
  :ensure
  :demand
  :diminish global-whitespace-mode
  :config
  (global-whitespace-mode 1))

(defun mugu-compile-all ()
  "Recompile every external package."
  (interactive)
  (byte-recompile-directory package-user-dir 0 'force))

;;; transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'mugu-core)

(provide 'mugu-core)
;;; mugu-core ends here
