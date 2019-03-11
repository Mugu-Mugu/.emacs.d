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
  (setq mugu-directory dir))

(defun mugu-directory-with-current-file-path ()
  "Update mugu-directory to current `default-directory'."
  (interactive)
  (mugu-directory-cd default-directory))

(defun mugu-directory-pwd ()
  "Return `mugu-directory'."
  mugu-directory)

(defun mugu-directory-pwd-file ()
  "Return `default-directory'."
  default-directory)

(defun mugu-directory-find-file-try-cd (dirname)
  "Change the current dir to DIRNAME if it's different from current one.
Return nil and do nothing otherwise."
  (when (not (equal (file-name-directory dirname)
                    (file-name-directory default-directory)))
    (mugu-directory-cd dirname)
    (let ((default-directory dirname))
      (call-interactively #'find-file))))

(defun mugu-directory-find-file-or-cd (pathname)
  "Cd to PATHNAME if its a dir or visit it otherwise."
  (message "mugu %s" pathname)
  (if (file-directory-p pathname)
      (mugu-directory-cd pathname)
    (find-file pathname)))

(provide 'mugu-directory)
;;; mugu-directory ends here
