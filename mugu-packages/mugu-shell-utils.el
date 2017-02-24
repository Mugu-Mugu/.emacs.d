;;; Package --- Summary
;; Provides utility functions for shell mode
;;; Commentary:

;;; Code:
(require 'shell)
(require 'mugu-core)
(require 'mugu-directory-fix)

;; on insert mode, autoscroll to end of buffer
(defun mugu-shell-scroll-before-insert ()
  "Scroll to prompt if needed."
  (interactive)
  (unless (equal (line-number-at-pos (point-max)) (line-number-at-pos))
    (comint-show-maximum-output)))

(defun mugu-shell-change-directory (select-dir-fun &rest args)
  "Wrapper to ensure the directory change will be safe.
directory is selected through SELECT-DIR-FUN on which is applied ARGS"
  (interactive)
  (comint-show-maximum-output)
  (comint-kill-input)
  (insert (concat "cd " (apply select-dir-fun args)))
  (mugu-directory-cd default-directory)
  (comint-send-input)
  (mugu-directory-with-current-file-path))

(defun mugu-shell-send-input ()
  "Ensure the command sent is safe (standard one may be dangerous because of evil)."
  (interactive)
  (comint-show-maximum-output)
  (comint-send-input))

(after 'evil
  ;;  modify change line behaviour to apply only on the prompt regardless of point location
  (defun mugu-shell-change-line ()
    (interactive)
    (mugu-shell-scroll-before-insert)
    (comint-bol)
    (call-interactively 'evil-change-line))
  (defun mugu-shell-change ()
    (interactive)
    (mugu-shell-scroll-before-insert)
    (call-interactively 'evil-change)))

(provide 'mugu-shell-utils)
;;; mugu-shell-utils ends here
