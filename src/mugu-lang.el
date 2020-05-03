;;; mugu-lang --- define common menu for all languages -*- lexical-binding: t -*-
;;; Commentary:
;; Client package should use the provided menu and remap command in the keymap
;; of the relevant major mode to bind the command

;;; Code:
(require 'mugu-menu)
(require 'general)

(defmacro define-mugu-lang-command (name)
  "Define a mugu-lang stub command named mugu-lang- NAME."
  `(defun ,(intern (format "mugu-lang-%s" name)) ()
     (interactive)
     "A stub command that should be remapped."
     (message "feature %s not available in %s" ,(symbol-name name) major-mode)))

(define-mugu-lang-command goto-def)
(define-mugu-lang-command find-ref)
(define-mugu-lang-command execute-code-action)
(define-mugu-lang-command format-buffer)
(define-mugu-lang-command rename-thing)
(define-mugu-lang-command test-file)
(define-mugu-lang-command test-rerun-last)
(define-mugu-lang-command test-toggle-goto)
(define-mugu-lang-command test-method)
(define-mugu-lang-command test-single-at-point)
(define-mugu-lang-command test-all-in-project)
(define-mugu-lang-command additional-menu)

(defmenu mugu-lang-menu (:color blue :hint nil)
  "-- Language menu -- "
  ("g" mugu-lang-goto-def "goto def" :column "Navigation")
  ("x" mugu-lang-find-ref "xref lookup")
  ("s" imenu "structure menu")
  ("f" mugu-lang-execute-code-action "fix code" :column "Actions")
  ("F" mugu-lang-format-buffer "fix file")
  ("r" mugu-lang-rename-thing "rename thing")
  ("t" mugu-lang-test-menu "test menu" :column "Submenu")
  ("a" mugu-lang-additional-menu "additional menu"))

(defmenu mugu-lang-test-menu (:color blue :hint nil)
  "-- Language menu test --"
  ("t" mugu-lang-test-file "verify test" :column "rspec")
  ("r" mugu-lang-test-rerun-last "rerun test")
  ("g" mugu-lang-test-toggle-goto "switch spec/file")
  ("m" mugu-lang-test-method "verify method")
  ("s" mugu-lang-test-single-at-point "verify example")
  ("a" mugu-lang-test-all-in-project "verify project"))

(defun mugu-lang-activate-for-mode (mode)
  "Activate the mugu lang mode for MODE."
  (general-define-key :keymaps (intern (format "%s-mode-map" mode))
                      [remap mugu-menu-call-mode-menu] #'mugu-lang-menu))


(defun mugu-lang--make-keymap ()
  "Build a keymap for the minor lang mode."
  (let ((map (make-sparse-keymap)))
    (define-key map [remap mugu-menu-call-mode-menu] #'mugu-lang-menu)
    map))

(defmacro mugu-define-lang-mode (lang doc)
  "Define a minor mode to provide bindings to interract with a language.
LANG is exepect to be the name of a major mode for a language.
DOC will be forwarded to the language minor mode documenation.
The created minor mode will contains:
* An interactive menu with predifined command with a set binding
* Eventually a default implementation for some commands.
Predifined command are usually stub that should be defined and remapped
according to capability of the defined language This ensure all language have
the same user interface."
  (let* ((mode-symbol (intern (format "mugu-%s-minor-mode" lang))))
    `(define-minor-mode ,mode-symbol ,(format "%s" doc)
       :keymap (mugu-lang--make-keymap))))

(provide 'mugu-lang)
;;; mugu-lang ends here
