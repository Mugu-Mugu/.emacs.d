;;; mugu-hydra-posframe --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'ivy)
(require 'posframe)
(require 'mugu-posframe)

(defun mugu-ivy-posframe--with-root-frame-advice (orig &rest args)
  "Advice to call ORIG `hydra-posframe-show' but in the root window.
Otherwise calling hydra in a posframe would be no good as size would be too
small.
ARGS are passed as it."
  (with-selected-frame (mugu-posframe-get-root-frame (selected-frame))
    (apply orig args)))

(defun mugu-ivy-posframe--activate ()
  "Setup for mugu-ivy-posframe-mode."
  (advice-add 'ivy--get-window :around #'mugu-ivy-posframe--with-root-frame-advice))

(defun mugu-ivy-posframe--deactivate ()
  "Tear down for mugu-ivy-posframe-mode."
  (advice-remove 'ivy--get-window #'mugu-ivy-posframe--with-root-frame-advice))

(define-minor-mode mugu-ivy-posframe-mode
  "Define mugu-ivy-posframe-mode."
  :global t
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-ivy-posframe-mode
      (mugu-ivy-posframe--activate)
    (mugu-ivy-posframe--deactivate)))

(provide 'mugu-ivy-posframe)
;;; mugu-ivy-posframe ends here
