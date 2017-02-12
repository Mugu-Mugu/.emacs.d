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
  (mugu-directory-cd default-directory))

(defun mugu-find-file-or-cd (vanilla-find-file filename &optional wildcards)
  "same as find file but will change directory if input is a dirctory"
  (if (and (file-directory-p filename)
           (not (equal filename mugu-directory-path)))
      (progn
        (mugu-directory-cd filename)
        (with-mugu-dir (lambda ()
                         (interactive)
                         (call-interactively (command-remapping 'find-file)))))
    (apply vanilla-find-file filename wildcards)))

(defun with-mugu-dir (fun)
  "Call FUN with mugu directory"
  (let ((default-directory mugu-directory-path))
    (call-interactively fun)))

(defun mugu-directory-pwd ()
  "return current mugu directory"
  mugu-directory-path)

(defun mugu-directory-pwd-file ()
  "return current directory of current file"
  default-directory)

(add-hook 'eshell-directory-change-hook 'mugu-directory-after-eshell-cd)
(advice-add 'cd :after #'mugu-directory-after-cd)
(advice-add 'find-file :around #'mugu-find-file-or-cd)

(provide 'mugu-directory-fix)
