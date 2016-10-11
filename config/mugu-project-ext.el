(defun mugu-project-find-dir ()
  "Change to a directory within the project with completion"
  (interactive)
  (let ((dir (projectile-complete-dir)))
    (mugu-directory-cd (expand-file-name dir (projectile-project-root)))))

(provide 'mugu-project-ext)
