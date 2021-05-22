;;; mugu-slack-ext --- Add various missing utilities to slack -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'dash)
(require 'slack-team)
(require 'slack-room)
(require 'tracking)
(require 'asoc)
(require 'slack-message-buffer)
(require 'mugu-menu)

(defun mugu-slack-ext-mark-as-read ()
  "Mark current buffer as read if it is a slack one."
  (interactive)
  (when (derived-mode-p 'slack-message-buffer-mode)
    (save-excursion
      (goto-char (point-max))
      (and slack-current-buffer
           (slack-buffer-update-mark-request slack-current-buffer
                                             (slack-buffer-latest-ts slack-current-buffer))))))


(defun mugu-slack-ext-next-unread ()
  "Go to next unread slack buffer and mark it as read."
  (interactive)
  (tracking-next-buffer)
  (mugu-slack-ext-mark-as-read)
  (mugu-menu-call-mode-menu))

(defun mugu-slack-ext-select-unread ()
  "Select an unread slack channel and mark it as read."
  (interactive)
  (slack-select-unread-rooms)
  (mugu-slack-ext-mark-as-read)
  (mugu-menu-call-mode-menu))

(defun mugu-slack-request-channel-info ()
  "Update a channel information and reset mentions."
  (interactive)
  (when (derived-mode-p 'slack-message-buffer-mode)
    (slack-conversations-info (slack-buffer-room slack-current-buffer)
                              (slack-buffer-team slack-current-buffer))))

(defun mugu-slack-ext--activate ()
  "Setup for mugu-slack-ext-mode."
  )

(defun mugu-slack-ext--deactivate ()
  "Tear down for mugu-slack-ext-mode."
  )

(define-minor-mode mugu-slack-ext-mode
  "Define mugu-slack-ext-mode which defines additional or tweaked command."
  :global t
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-slack-ext-mode
      (mugu-slack-ext--activate)
    (mugu-slack-ext--deactivate)))

(provide 'mugu-slack-ext)
;;; mugu-slack-ext ends here
