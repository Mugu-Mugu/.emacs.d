;; to prevent this annoying emacs behaviour to change working directory when a buffer is loaded
(defvar mugu-directory-path user-emacs-directory)
(defun mugu-directory-fix-dir () (setq default-directory mugu-directory-path))
(defun mugu-directory-after-eshell-cd () (setq mugu-directory-path default-directory))
(defun mugu-directory-after-cd (&rest args) (cond ((called-interactively-p 'interactive)
                                                   (setq mugu-directory-path default-directory))))
(defun mugu-directory-after-find-file () (setq default-directory mugu-directory-path))

(defun mugu-directory-cd (dir)
  "for non interactive cd"
  (progn
    (setq default-directory dir)
    (setq mugu-directory-path dir)))

(defun mugu-directory-with-current-file-path ()
  "update directory to path of current file"
  (interactive)
  (mugu-directory-cd (file-name-directory buffer-file-name))
  )

(add-hook 'buffer-list-update-hook 'mugu-directory-fix-dir)
(add-hook 'eshell-directory-change-hook 'mugu-directory-after-eshell-cd)
(add-hook 'find-file-hooks 'mugu-directory-after-find-file)
(advice-add 'cd :after #'mugu-directory-after-cd)

(provide 'mugu-directory-fix)
