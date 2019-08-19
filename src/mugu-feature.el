;;; mugu-feature --- Summary
;; Provide abstract features using what package has been loaded already.
;;; Commentary:
;; This is to ease required dependencies of custom package

;;; Code:
(require 'dash)

(defun mugu-feature--select-method (&rest methods)
  "Return the first method of METHODS that is defined."
  (-first-item
   (-non-nil
    (--map (when (fboundp it) it)
           methods))))

(defun mugu-feature-switch-buffer (&optional buffer)
  "Switch to BUFFER using the most appropriate method.
If BUFFER is nil, an interactive prompt will be provided."
  (interactive)
  (if buffer
      (funcall (mugu-feature--select-method 'mugu-project-switch-buffer
                                            'switch-to-buffer)
               buffer)

    (call-interactively (mugu-feature--select-method 'mugu-project-switch-buffer
                                                     'ivy-switch-buffer
                                                     'switch-to-buffer))))

(defun mugu-feature-pop-to-buffer (buffer &rest args)
  "Pop to BUFFER.
ARGS are unspecified."
  (interactive)
  (apply (mugu-feature--select-method 'mugu-project-pop-to-buffer
                                        'pop-to-buffer)
         buffer args))

(provide 'mugu-feature)
;;; mugu-feature ends here
