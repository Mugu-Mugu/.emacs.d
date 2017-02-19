;;; mugu-motion-utils --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'avy)
(require 'evil-integration)

;;;###autoload
(defun mugu-motion-f (char)
  "Jump (inclusive) to the currently visible right CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     (regexp-quote (string char))
     avy-all-windows
     avy-style
     (point)
     (line-end-position))))

;;;###autoload
(defun mugu-motion-F (char)
  "Jump (inclusive) to the currently visible left CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     (regexp-quote (string char))
     avy-all-windows
     avy-style
     (line-beginning-position)
     (point))))

;;;###autoload
(defun mugu-motion-t (char)
  "Jump (exclusive) to the currently visible right CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (let ((initial-point (point)))
    (mugu-motion-f char)
    (unless (equal initial-point (point))
      (backward-char))))

;;;###autoload
(defun mugu-motion-T (char)
  "Jump (exclusive) to the currently visible left CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (let ((initial-point (point)))
    (mugu-motion-F char)
    (unless (equal initial-point (point))
      (forward-char))))

(evil-define-avy-motion mugu-motion-t exclusive)
(evil-define-avy-motion mugu-motion-T exclusive)
(evil-define-avy-motion mugu-motion-f inclusive)
(evil-define-avy-motion mugu-motion-F inclusive)

(provide 'mugu-motion-utils)
;;; mugu-motion-utils ends here
