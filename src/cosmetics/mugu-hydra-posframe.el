;;; mugu-hydra-posframe --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'posframe)
(require 'mugu-posframe)

(defun mugu-hydra-posframe--with-root-frame-advice (orig &rest args)
  "Advice to call ORIG `hydra-posframe-show' but in the root window.
Otherwise calling hydra in a posframe would be no good as size would be too
small.
ARGS are passed as it."
  (with-selected-frame (mugu-posframe-get-root-frame (selected-frame))
    (apply orig args)))

(defun mugu-hydra-posframe--activate ()
  "Setup for mugu-hydra-posframe-mode."
  (advice-add 'hydra-posframe-show :around #'mugu-hydra-posframe--with-root-frame-advice)
  (setq hydra-posframe-show-params
        `(:internal-border-width 1
         :internal-border-color "gray50"
         :poshandler ,#'posframe-poshandler-frame-bottom-center
         :left-fringe 10
         :right-fringe 10))
  (setq hydra-hint-display-type 'posframe))

(defun mugu-hydra-posframe--deactivate ()
  "Tear down for mugu-hydra-posframe-mode."
  (advice-remove 'hydra-posframe-show #'mugu-hydra-posframe--with-root-frame-advice)
  (custom-reevaluate-setting 'hydra-hint-display-type))

(define-minor-mode mugu-hydra-posframe-mode
  "Define mugu-hydra-posframe-mode."
  :global t
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-hydra-posframe-mode
      (mugu-hydra-posframe--activate)
    (mugu-hydra-posframe--deactivate)))

(provide 'mugu-hydra-posframe)
;;; mugu-hydra-posframe ends here
