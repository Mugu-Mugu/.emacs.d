;;; mugu-slack-wconf-tab --- Configure specific rules for slack buffer with TAB -*- lexical-binding: t -*-
;;; Commentary:

(require 'mugu-tab)
(require 'mugu-slack-wconf)
(require 'mugu-window)

;;; Code:
(defconst mugu-slack-wconf-tab-name
  "slack"
  "Name of the tab reserved for wconf buffer.")

(defun mugu-slack-wconf-tab--eligible-buffer-p (buffer-name &rest _)
  "Predicate indicating if BUFFER-NAME is eligible for TAB display buffer rule."
  (and (s-equals? mugu-slack-wconf-tab-name (mugu-tab-current-tab-name))
       (mugu-slack-wconf-slack-buffer-p buffer-name)))

(defun mugu-slack-wconf-tab--activate ()
  "."
  (mugu-slack-wconf-mode -1)
  (mugu-slack-wconf-mode +1)
  (mugu-window-configure-normal-window #'mugu-slack-wconf-tab--eligible-buffer-p)
  (save-current-tab
   (mugu-tab-new)
   (mugu-tab-rename mugu-slack-wconf-tab-name)))

(defun mugu-slack-wconf-tab--deactivate ()
  "."
  (mugu-window-remove-display-rule #'mugu-slack-wconf-tab--eligible-buffer-p)
  (save-current-tab
   (mugu-tab-try-delete mugu-slack-wconf-tab-name)))

(define-minor-mode mugu-slack-wconf-tab-mode
  "Handle display configuration for slack files when `mugu-tab' active."
  :global t
  :group 'mugu
  (if mugu-slack-wconf-tab-mode
      (mugu-slack-wconf-tab--activate)
    (mugu-slack-wconf-tab--deactivate)))

(provide 'mugu-slack-wconf-tab)
;;; mugu-org-wconf-tab ends here
