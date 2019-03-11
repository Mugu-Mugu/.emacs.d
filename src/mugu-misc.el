;;; mugu-misc --- Summary
;; a collections of function that are too general to be bundled in a specific package
;;; Commentary:

;;; Code:

;; * begin:
(defun newline-without-break-of-line ()
  "1. move to end of the line.
2. open new line and move to new line"
  (interactive)
  (end-of-line)
  (open-line 1)
  (right-char))

(defun mugu-scroll-lines (increment)
  "Scroll by INCREMENT lines.
if INCREMENP is negative, it scrolls up."
  (interactive)
  (let ((scroll-preserve-screen-position 1))
    (scroll-up increment)))

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; super escape
;; to improve
;;; pluggin mode activation
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(provide 'mugu-misc)
;;; mugu-misc ends here
