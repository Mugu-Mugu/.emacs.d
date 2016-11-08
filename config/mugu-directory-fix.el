;; to prevent this annoying emacs behaviour to change working directory when a buffer is loaded
(defvar mugu-directory-path user-emacs-directory)
(defun mugu-directory-after-eshell-cd () (setq mugu-directory-path default-directory))
(defun mugu-directory-after-cd (&rest args) (cond ((called-interactively-p 'interactive)
                                                   (setq mugu-directory-path default-directory))))

(defun mugu-directory-cd (dir)
  "for non interactive cd"
    (setq mugu-directory-path dir))

(defun mugu-directory-with-current-file-path ()
  "update directory to path of current file"
  (interactive)
  (mugu-directory-cd (file-name-directory buffer-file-name)))

(defun with-mugu-dir (fun)
  "Call FUN with mugu directory"
  (let ((old-directory default-directory))
    (setq default-directory mugu-directory-path)
    (with-demoted-errors "%s" (call-interactively fun))
    (setq default-directory old-directory)))

(defun mugu-directory-pwd ()
  "return current mugu directory"
  mugu-directory-path)

(defun mugu-directory-pwd-file ()
  "return current directory of current file"
  default-directory)

(add-hook 'eshell-directory-change-hook 'mugu-directory-after-eshell-cd)
(advice-add 'cd :after #'mugu-directory-after-cd)

(provide 'mugu-directory-fix)
