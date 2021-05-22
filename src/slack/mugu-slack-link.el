;;; mugu-slack-link --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'link-hint)
(require 'slack-file-info-buffer)
(require 'slack-buffer)



(defun mugu-slack-link--file-link-at-point-p ()
  "Determine if thing at point is a file link for slack."
  (and (get-text-property (point) 'file)
       slack-current-buffer))

(defun mugu-slack-link--next-file-link (&optional bound)
  "Return the next file link until BOUND."
  (link-hint--next-property 'file bound))

(defun mugu-slack-link--activate ()
  "Setup for mugu-slack-link-mode."
  (link-hint-define-type 'slack-file-link
    :next #'mugu-slack-link--next-file-link
    :at-point-p #'mugu-slack-link--file-link-at-point-p
    :open #'slack-file-display
    :open-multiple t
    :copy #'kill-new)
  (push 'link-hint-slack-file-link link-hint-types))

(defun mugu-slack-link--deactivate ()
  "Tear down for mugu-slack-link-mode."
  (setq link-hint-types (remove 'link-hint-slack-file-link link-hint-types)))

(define-minor-mode mugu-slack-link-mode
  "Define mugu-slack-link-mode."
  :global t
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-slack-link-mode
      (mugu-slack-link--activate)
    (mugu-slack-link--deactivate)))


(provide 'mugu-slack-link)
;;; mugu-slack-link ends here
