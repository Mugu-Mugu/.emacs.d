;;; mugu-posframe --- Extension to posframe -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'posframe)
(require 'asoc)

(defun mugu-posframe-get-root-frame (frame)
  "Return the root parent of FRAME."
  (if-let (parent (frame-parent frame))
      (mugu-posframe-get-root-frame parent)
    frame))

(defun mugu-posframe-with-root-frame-parent-params ()
  "Return a plist of argument for `posframe-show' where parent frame will be root."
  (let ((parent-frame (mugu-posframe-get-root-frame (selected-frame))))
    `(:poshandler-extra-info (:parent-frame
                              ,parent-frame
                              :parent-frame-width
                              ,(frame-pixel-width parent-frame)
                              :parent-frame-height
                              ,(frame-pixel-height parent-frame)))))

(defun mugu-display-buffer-in-posframe (buffer alist)
  "This is a `display-buffer' action that prints BUFFER in a posframe.
ALIST has the same parameter has `display-buffer' but some parameter may have
no effect and some have been added:
 `with-focus' -- t if the created posframe should have focus
 `posframe-params' -- alist of parameters (refer to `posframe-show')"
  (when (posframe-workable-p)
    (let ((posframe (posframe-show buffer
                                   :accept-focus t
                                   :string nil
                                   :poshandler 'posframe-poshandler-point-window-center)))
      (when (asoc-get alist 'with-focus)
        (select-frame-set-input-focus posframe)))))

(provide 'mugu-posframe)
;;; mugu-posframe ends here
