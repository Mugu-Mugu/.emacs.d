;;; mugu-feature --- Allows to declare features -*- lexical-binding: t -*-
;;; Commentary:
;;; Feature can be preemtively declared and binded in a coherent set regardless of actual implementation.
;;; Mode can later choose to rebind a particalar feature to implement it or just tweak it as needed without
;;; consideration for binding choice or conflict

;;; Code:
(defmacro define-mugu-feature (name)
  "Define a mugu-feature stub command named mugu-feature- NAME."
  `(defun ,(intern (format "mugu-feature-%s" name)) ()
     (interactive)
     "A stub command that should be remapped."
     (message "feature %s not defined" ,(symbol-name name))))

(define-mugu-feature search-google)
(define-mugu-feature search-google-at-point)
(define-mugu-feature slack)

(provide 'mugu-feature)
;;; mugu-feature ends here
