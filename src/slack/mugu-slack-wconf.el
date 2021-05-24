;;; mugu-slack-wconf --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

(require 'slack)
(require 'mugu-window)
;;; Code:

(defun mugu-slack-wconf-slack-buffer-p (buffer-name &rest _)
  "Predicate indicating if BUFFER-NAME is a slack-buffer."
  (equal (buffer-local-value 'major-mode (get-buffer buffer-name)) 'slack-message-buffer-mode))

(defmacro def-slack-buffer-mode-p (predicate mode)
  "Create a PREDICATE indicating if MODE is active in a buffer."
  `(defun ,(intern (format "mugu-slack-wconf-%s-p" predicate)) (buffer &rest _alist)
     ,(format "Predicate indicating if BUFFER is in %s" mode)
     (with-current-buffer buffer
       (derived-mode-p ,mode))))

(def-slack-buffer-mode-p message-buffer 'slack-message-buffer-mode)
(def-slack-buffer-mode-p message-edit-buffer 'slack-message-edit-buffer-mode)
(def-slack-buffer-mode-p message-thread-buffer 'slack-thread-message-buffer-mode)
(def-slack-buffer-mode-p file-buffer 'slack-file-info-buffer-mode)
(def-slack-buffer-mode-p file-buffer 'slack-file-info-buffer-mode)

(defun mugu-slack-wconf-buffer-mode-p (mode)
  "Return a closure predicate determining if an input buffer has MODE."
  (lambda (buffer-name &rest _) (equal (buffer-local-value 'major-mode (get-buffer buffer-name)) mode)))

(defun mugu-slack-wconf--configure (is-activation)
  "Activate wconf if IS-ACTIVATION is true.  Deactivate otherwise."
  (let ((func (if is-activation #'mugu-window-configure-side-window #'mugu-window-remove-display-rule)))
    (funcall func #'mugu-slack-wconf-slack-buffer-p 'left 0.4)
    (funcall func #'mugu-slack-wconf-message-edit-buffer-p 'top 0.2)
    (funcall func #'mugu-slack-wconf-message-thread-buffer-p 'right 0.4)
    (funcall func #'mugu-slack-wconf-file-buffer-p 'right 0.4)
    (funcall func ".*Slack.*Profile.*" 'right 0.3)
    (funcall func ".*Slack.*:.*Files.*" 'right 0.3)
    (funcall func ".*Slack.*:.*Stars.*" 'right 0.3)))

(define-minor-mode mugu-slack-wconf-mode
  "Handle display configuration for-slack files."
  :global t
  :group 'mugu
  (if mugu-slack-wconf-mode
      (mugu-slack-wconf--configure 'activation)
    (mugu-slack-wconf--configure nil)))

(provide 'mugu-slack-wconf)
;;; mugu-slack-wconf ends here
