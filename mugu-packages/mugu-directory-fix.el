;;; Package --- Summary
;; To prevent this annoying emacs behaviour where the default-directory is tied
;; to the current buffer, this package defines an alternative
;; 'default-directory' and means to synchronize it to the real one. It thus
;; becomes possible to control where a command or a function is done thanks to
;; the `with-mugu-dir' macro.
;;; Commentary:

;;; Code:
(defvar mugu-directory user-emacs-directory
  "The alternative and controllable `default-directory'.")

(defmacro with-mugu-dir (&rest body)
  "Execute the forms in BODY with mugu-directory as default directory."
  (declare (indent 1) (debug t))
  `(let ((default-directory mugu-directory))
     ,@body))


(defun mugu-directory-cd (dir)
  "Set mugu directory to DIR.
Doesnt change default directory of current buffer"
  (setq mugu-directory  dir))

(defun mugu-directory-with-current-file-path ()
  "Update mugu-directory to current `default-directory'."
  (interactive)
  (mugu-directory-cd default-directory))

(defun mugu-find-file-or-cd (vanilla-find-file filename &optional wildcards)
  "Advice to `find-file' with identical purpose but may update `mugu-directory'.
VANILLA-FIND-FILE is `find-file'.
FILENAME is as in `find-file'.
WILDCARDS is as in `find-file'.
if FILENAME is a directory and happens to be different from `mugu-directory'
instead of calling `find-file', `mugu-directory' will be updated instead to
FILENAME.  Otherwise behaviour is the same."
  (if (and (file-directory-p filename)
           (not (equal filename mugu-directory)))
      (progn
        (mugu-directory-cd filename)
        (with-mugu-dir
            (call-interactively (command-remapping 'find-file))))
    (apply vanilla-find-file filename wildcards)))

(defun mugu-directory-pwd ()
  "Return `mugu-directory'."
  mugu-directory)

(defun mugu-directory-pwd-file ()
  "Return `default-directory'."
  default-directory)

(advice-add 'find-file :around #'mugu-find-file-or-cd)

(provide 'mugu-directory-fix)
;;; mugu-directory-fix ends here
