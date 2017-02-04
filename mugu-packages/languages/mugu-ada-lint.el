;;; this is an adapted copy of the ada-gnat checker from flycheck
;;; this one supports gpr files and should thus yield the same warnings
;;; and errors that a project compilation would
;;; this means no more xxx files was not found or false positives because
;;; wrong file or warning switch was used


(flycheck-def-args-var mugu-ada-lint-args ada-mugu
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var mugu-ada-lint-lang-rev "2012" ada-mugu
  "The language standard to use in GNAT.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard. When non-nil, pass
the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck "2012" ada-mugu
  "The language standard to use in GNAT.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard. When non-nil, pass
the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker ada-mugu
  "An Ada syntax checker using GPR

Uses the GNAT compiler from GCC.  See URL
`http://libre.adacore.com/tools/gnat-gpl-edition/'."
  :command ("gprbuild"
            "-c"                        ; Just compile, don't bind
            "-gnatc"                    ; no object generated, semantic checks only
            "-f"                        ; Force re-compilation
            "-u"                        ; Compile the current file only
            "-gnatf"                    ; Full error information
            "-gnatef"                   ; Full source file name
            (eval (concat "-P" ada-prj-current-file))
            (eval mugu-ada-lint-args)
            source-original)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ;no specific error prefix in Ada
          ": " (message) line-end))
  :modes ada-mode
  :predicate
  ;;; a project must be defined and checked file must be part of it
  (lambda ()
    (condition-case err
        (progn
          (ada-require-project-file)
          (ada-check-current-project (buffer-file-name))
          t)
      (error (message "%s" (error-message-string err)))
      )))
                                    
      
(provide 'mugu-ada-lint)
