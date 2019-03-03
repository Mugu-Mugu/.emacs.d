;;; mugu-motion-utils --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'avy)
(require 'evil-integration)

(defun mugu-motion-t (char)
  "Jump (exclusive) to the currently visible right CHAR in the current line."
  (interactive (list (read-char)))
  (let ((initial-point (point)))
    (avy-goto-char-in-line char)
    (cond ((< (point) initial-point) (forward-char))
          ((> (point) initial-point) (backward-char)))))

(defun mugu-motion-f (char)
  "Quieter `avy-goto-char-in-line' (don't prompt for CHAR)."
  (interactive (list (read-char)))
  (avy-goto-char-in-line char))

(evil-define-avy-motion mugu-motion-t inclusive)
(evil-define-avy-motion mugu-motion-f inclusive)

(provide 'mugu-motion-utils)
;;; mugu-motion-utils ends here
