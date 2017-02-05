(require 'bookmark)
(require 'mugu-directory-fix)
(require 'ivy)

(defun mugu-bookmark-register-dir ()
  "Register a bookmark on a directory interactly."
  (interactive)
  (let ((dir-to-bookmark default-directory))
    ;; ugly, with-mugu-dir should return result of fun
    (with-mugu-dir (lambda ()
                     (interactive)
                     (setq dir-to-bookmark (expand-file-name
                                            (read-directory-name "select dir to bookmark")))))
    (save-window-excursion
      (dired dir-to-bookmark)
      (ivy-read "Select bookmark name" (bookmark-all-names)
                :action (lambda (x) (bookmark-set x))
                :caller 'counsel-bookmark))))

(defun mugu-bookmark-load-dir ()
  (interactive)
  (require 'cl-lib)
  (ivy-read "Select bookmark name" 
            (cl-remove-if-not
             (lambda (x) (file-directory-p (bookmark-location x)))
             (bookmark-all-names))
            :action (lambda (x) (mugu-directory-cd (bookmark-location x)))
            :caller 'counsel-bookmark))

(defun mugu-bookmark-load-file ()
  (interactive)
  (require 'cl-lib)
  (ivy-read "Select bookmark name" 
            (cl-remove-if-not
             (lambda (x) (file-regular-p (bookmark-location x)))
             (bookmark-all-names))
            :action (lambda (x) (find-file (bookmark-location x)))
            :caller 'counsel-bookmark))

(provide 'mugu-bookmark-utils)
