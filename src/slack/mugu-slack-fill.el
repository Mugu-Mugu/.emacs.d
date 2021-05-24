;;; mugu-slack-fill --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
;;; Simple minor mode for a saner display for long line in slack buffers
;;; Currently the wrap is dumb and does not account for indentation of
;;; current line but this could be easily fixed

;;; Code:
(require 'lui)
(require 'simple)
(require 'visual-fill-column)

(defcustom mugu-slack-fill-column 100
  "Max column size of a line in slack.
Line larger than this will be visually wrapped."
  :type '(number :tag "Max column size")
  :group 'mugu)

(defun mugu-slack-fill--activate ()
  "Setup for mugu-slack-fill-mode."
  (visual-line-mode +1)
  (visual-fill-column-mode +1)
  (setq-local lui-time-stamp-position 'right-margin)
  (setq-local visual-fill-column-width mugu-slack-fill-column)
  (setq-local lui-fill-column mugu-slack-fill-column)
  (setq-local lui-fill-type nil))

(defun mugu-slack-fill--deactivate ()
  "Tear down for mugu-slack-fill-mode."
  (visual-line-mode -1)
  (visual-fill-column-mode -1)
  (kill-local-variable 'lui-time-stamp-position)
  (kill-local-variable 'visual-fill-column-width)
  (kill-local-variable 'lui-fill-colum)
  (kill-local-variable 'lui-fill-type))

(define-minor-mode mugu-slack-fill-mode
  "Define mugu-slack-fill-mode."
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-slack-fill-mode
      (mugu-slack-fill--activate)
    (mugu-slack-fill--deactivate)))

(provide 'mugu-slack-fill)
;;; mugu-slack-fill ends here
