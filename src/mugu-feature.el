;;; mugu-feature --- Allows to declare features -*- lexical-binding: t -*-
;;; Commentary:
;;; Feature can be preemtively declared and binded in a coherent set regardless of actual implementation.
;;; Mode can later choose to rebind a particalar feature to implement it or just tweak it as needed without
;;; consideration for binding choice or conflict

;;; Code:
(defmacro define-mugu-feature (name &optional documentation)
  "Define a mugu-feature stub command named mugu-feature- NAME.
DOCUMENTATION may be specified and will be injected in command definition."
  `(defun ,(intern (format "mugu-feature-%s" name)) ()
     (interactive)
     ,(format "%s." (or documentation "A stub command that should be remapped"))
     (message "feature %s not defined" ,(symbol-name name))))

;; features entry point
(define-mugu-feature space "Entry point for toplevel bindings and feature")
(define-mugu-feature major-mode-space "Entry point for major mode bindings")
(define-mugu-feature slack "Entry point for slack bindings")
(define-mugu-feature project "Entry point for project bindings")
(define-mugu-feature spelling "Entry point for spelling bindings")
(define-mugu-feature organization "Entry point for organization bindings")
(define-mugu-feature window "Entry point for window management bindings")
(define-mugu-feature workspace "Entry point for workspace bindings")
(define-mugu-feature versioning "Entry point for versionning bindings")
(define-mugu-feature help "Entry point for help bindings")

;; top-level command
(define-mugu-feature execute "Execute a command")
(define-mugu-feature display-yank-ring)
(define-mugu-feature goto-link)
(define-mugu-feature create-link)

;;; find files
(define-mugu-feature find-file)
(define-mugu-feature find-file-recursive)
(define-mugu-feature find-directory-recursive)
(define-mugu-feature find-buffer)

;; search things
(define-mugu-feature search-web "Search something on the web")
(define-mugu-feature search-web-at-point "Search thing at point on the web")

(define-mugu-feature pop-terminal "Start a persistent terminal")
(define-mugu-feature pop-transient-terminal "Pop a terminal at `default-directory'")

(define-mugu-feature pop-binding-description "Display the applicable bindings for the current situation")

(provide 'mugu-feature)
;;; mugu-feature ends here
