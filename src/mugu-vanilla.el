;;; mugu-vanilla --- Summary
;; a collection of possible configuration for vanilla emacs settings
;;; Commentary:

;;; Code:
(require 'mugu-cosmetics)

;; * begin:
(defun mugu-vanilla--gc ()
  "."
  (garbage-collect))

(defun mugu-vanilla--kill-scratch ()
  "Scratch buffer is hard coded to Emacs Lisp mode.
Loading it in init would trigger prematuraly the Emacs Lisp configuration."
  (kill-buffer "*scratch*"))

(defsubst mugu-vanilla-set-startup-settings ()
  "."
  (setq initial-frame-alist (mugu-cosmetics-frame-params))
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'fundamental-mode)
  (setq inhibit-splash-screen nil
        inhibit-startup-echo-area-message t
        inhibit-startup-message t)

  (add-hook 'after-init-hook #'mugu-vanilla--kill-scratch))

(defsubst mugu-vanilla-set-encoding-settings ()
  "."
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(defsubst mugu-vanilla-set-backup-settings ()
  "."
  (setq create-lockfiles nil)
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq auto-save-list-file-prefix nil)
  (global-auto-revert-mode 1))

(defsubst mugu-vanilla-set-visual-settings ()
  "."
  (setq default-frame-alist (mugu-cosmetics-frame-params))
  (visual-line-mode 1)
  (global-hl-line-mode t)
  (set-face-background 'hl-line "#3e4446")
  (column-number-mode)
  (setq ring-bell-function 'ignore))

(defsubst mugu-vanilla-set-perf-settings ()
  "."
  (setq-default bidi-display-reordering nil)
  (setq gc-cons-threshold 5000000000)
  (add-hook 'focus-out-hook #'mugu-vanilla--gc))

(defsubst mugu-vanilla-set-other-settings ()
  "Leftover that could not be tied to a particular feature."
  (setq mark-ring-max 64)
  (setq global-mark-ring-max 128)
  (setq-default indent-tabs-mode nil)
  (setq require-final-newline t)
  (setq scroll-conservatively 9999
        scroll-preserve-screen-position t)
  (which-function-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p))

(provide 'mugu-vanilla)
;;; mugu-vanilla ends here