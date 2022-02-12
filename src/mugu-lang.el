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

;; tests
(define-mugu-lang-command test-file)
(define-mugu-lang-command test-rerun-last)
(define-mugu-lang-command test-toggle-goto)
(define-mugu-lang-command test-method)
(define-mugu-lang-command test-single-at-point)
(define-mugu-lang-command test-all-in-project)
(define-mugu-lang-command test-directory)
(define-mugu-lang-command test-debugger)

;; menus stubs
(define-mugu-lang-command additional-menu)
(define-mugu-lang-command lsp-menu)

;; lsp/general lang features
(define-mugu-lang-command find-symbol)
(define-mugu-lang-command find-declaration)
(define-mugu-lang-command find-definition)
(define-mugu-lang-command find-implementation)
(define-mugu-lang-command find-reference)
(define-mugu-lang-command find-type-definition)
(define-mugu-lang-command organize-imports)
(define-mugu-lang-command peek-find-implementation)
(define-mugu-lang-command peek-find-references)
(define-mugu-lang-command peek-find-definitions)
(define-mugu-lang-command peek-find-workspace-symbol)
(define-mugu-lang-command execute-code-action)
(define-mugu-lang-command format-buffer)
(define-mugu-lang-command format-region)
(define-mugu-lang-command rename-thing)
(define-mugu-lang-command doc-show-at-point)
(define-mugu-lang-command doc-search)
(define-mugu-lang-command doc-toc)

;; non lsp lang features
(define-mugu-lang-command compile)

(defmenu mugu-lang-menu (:color blue :hint nil)
  "-- Language menu -- "
  ("ga" mugu-lang-find-symbol "find symbol in workspace" :column "Find")
  ("gd" mugu-lang-find-declaration "find declarations")
  ("gg" mugu-lang-find-definition "find definitions")
  ("gi" mugu-lang-find-implementation "find implementations")
  ("gr" mugu-lang-find-reference "find references")
  ("gt" mugu-lang-find-type-definition "find type definition")
  ("s" imenu "structure menu" :column "Misc")

  ("ro" mugu-lang-organize-imports "organize imports" :column "Actions")
  ("rr" mugu-lang-rename-thing "rename")
  ("ra" mugu-lang-execute-code-action "code actions")
  ("==" mugu-lang-format-buffer "format buffer")
  ("=r" mugu-lang-format-region "format region")
  ("c" mugu-lang-compile "compile project")

  ("pg" mugu-lang-peek-find-definitions  "peek definitions" :column "Peek")
  ("pi" mugu-lang-peek-find-implementation  "peek implementations")
  ("pr" mugu-lang-peek-find-references  "peek references")
  ("ps" mugu-lang-peek-find-workspace-symbol  "peek workspace symbol")

  ("dd" mugu-lang-doc-show-at-point "show doc at point" :column "doc")
  ("ds" mugu-lang-doc-search "search in docs")
  ("df" mugu-lang-doc-toc "search a doc")

  ("l" mugu-lang-lsp-menu "lsp menu" :column "Submenus")
  ("t" mugu-lang-test-menu "test menu")
  ("a" mugu-lang-additional-menu "additional menu"))

(defmenu mugu-lang-test-menu (:color blue :hint nil)
  "-- Language menu test --"
  ("t" mugu-lang-test-file "verify test" :column "launch tests")
  ("r" mugu-lang-test-rerun-last "rerun test")
  ("g" mugu-lang-test-toggle-goto "switch spec/file")
  ("m" mugu-lang-test-method "verify method")
  ("s" mugu-lang-test-single-at-point "verify example")
  ("a" mugu-lang-test-all-in-project "verify project")
  ("d" mugu-lang-test-directory "test directory" :column "Submenu")
  ("D" mugu-test-debugger "start debugger" :column "debugger"))

(defun mugu-lang--activate ()
  "Setup for mugu-lang-mode."
  ;; I don't think this is ever the trouble with lsp or rg as fallback
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 99 'local)
  (remove-hook 'xref-backend-functions #'etags--xref-backend 'local)
  ;; yeah i dunno why it gets added but it does
  (remove-hook 'xref-backend-functions t 'local)
  (general-define-key :keymaps 'mugu-lang-mode-map
                      [remap mugu-menu-call-mode-menu] #'mugu-lang-menu))

(defun mugu-lang--deactivate ()
  "Tear down for mugu-lang-mode.")

(define-minor-mode mugu-lang-mode
  "Define mugu-lang-mode."
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-lang-mode
      (mugu-lang--activate)
    (mugu-lang--deactivate)))

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
       :keymap (make-sparse-keymap))))

(provide 'mugu-lang)
;;; mugu-lang ends here
