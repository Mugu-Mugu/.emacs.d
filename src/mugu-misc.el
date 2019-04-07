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

(defun mugu-delete-file (&optional file)
  "Delete the FILE and kill its associated buffer if any."
  (interactive (list (read-file-name "File to delete: ")))
  (let ((associated-buffer (get-file-buffer file)))
    (when associated-buffer (kill-buffer associated-buffer))
    (delete-file file)))

(defun mugu-delete-current-file ()
  "Delete the current buffer and its file if any."
  (interactive)
  (if (buffer-file-name)
      (mugu-delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun mugu-rename-file (&optional old-file new-file)
  "Sane rename-file: better interactive selection and reflect on buffer.
OLD-FILE renamed to NEW-FILE."
  (interactive (let* ((old-file (read-file-name "File to rename: " default-directory (buffer-file-name) 'must-match (buffer-file-name)))
                      (new-file (read-file-name (format "Rename %s to: " old-file) default-directory (buffer-file-name) nil (file-name-base (buffer-file-name)))))
                 (list old-file new-file)))
  (rename-file old-file new-file)
  (when (get-file-buffer old-file)
    (kill-buffer (get-file-buffer old-file))
    (find-file new-file)))

(defun mugu-rename-current-file ()
  "Rename current file or buffer if this is not tied to an actual file."
  (interactive)
  (if (buffer-file-name)
       (mugu-rename-file (buffer-file-name))
    (rename-buffer (read-buffer "New name for current buffer: " (buffer-name) nil))))

(provide 'mugu-misc)
;;; mugu-misc ends here
