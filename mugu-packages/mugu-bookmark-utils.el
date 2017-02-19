(require 'bookmark)
(require 'mugu-directory-fix)
(require 'ivy)

(defun mugu-bookmark-register-dir ()
  "Register a bookmark on a directory interactly."
  (interactive)
  ;; ugly, with-mugu-dir should return result of fun
(let ((dir-to-bookmark default-directory))
  (with-mugu-dir
      (setq dir-to-bookmark (expand-file-name (read-directory-name "select dir to bookmark"))))
  (save-window-excursion
    (dired dir-to-bookmark)
    (ivy-read "Select bookmark name" (bookmark-all-names)
     :action (lambda (x) (bookmark-set x))
     :caller 'counsel-bookmark))))

(provide 'mugu-bookmark-utils)
