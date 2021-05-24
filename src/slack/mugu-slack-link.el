;;; mugu-slack-link --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'link-hint)
(require 'slack-file-info-buffer)
(require 'slack-buffer)
(require 'button)

(defun mugu-slack-link--file-link-at-point-p ()
  "Determine if thing at point is a file link for slack."
  (and (get-text-property (point) 'file)
       slack-current-buffer))

(defun mugu-slack-link--next-file-link (&optional bound)
  "Return the next file link until BOUND."
  (link-hint--next-property 'file bound))

(defun mugu-slack-link--link-button-p ()
  "Determine if thing at point is a button with a link."
  (and (button-at (point))
       slack-current-buffer))

(defun mugu-slack-link--next-link-button (&optional _bound)
  "Return the position of the next link button."
  (and (forward-button 1 nil nil 'no-error)
       (point)))

(defun mugu-slack-link--activate ()
  "Setup for mugu-slack-link-mode."
  (make-local-variable 'link-hint-types)
  (link-hint-define-type 'slack-file-link
    :next #'mugu-slack-link--next-file-link
    :at-point-p #'mugu-slack-link--file-link-at-point-p
    :open #'slack-file-display
    :open-multiple t
    :copy #'kill-new)
  (link-hint-define-type 'slack-button-link
    :next #'mugu-slack-link--next-link-button
    :at-point-p #'mugu-slack-link--link-button-p
    :open #'push-button
    :open-multiple t
    :copy #'kill-new)
  (push 'link-hint-slack-file-link link-hint-types)
  (push 'link-hint-slack-button-link link-hint-types))

(defun mugu-slack-link--deactivate ()
  "Tear down for mugu-slack-link-mode."
  (kill-local-variable 'link-hint-types)
  ;; (setq link-hint-types (remove 'link-hint-slack-file-link link-hint-types))
  ;; (setq link-hint-types (remove 'link-hint-slack-button-link link-hint-types))
  )

(define-minor-mode mugu-slack-link-mode
  "Define mugu-slack-link-mode."
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-slack-link-mode
      (mugu-slack-link--activate)
    (mugu-slack-link--deactivate)))


(provide 'mugu-slack-link)
;;; mugu-slack-link ends here
