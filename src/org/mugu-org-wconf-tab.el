;;; mugu-org-wconf-tab --- Configure specific rules for org buffer with TAB -*- lexical-binding: t -*-
;;; Commentary:

(require 'mugu-tab)
(require 'mugu-org-wconf)
(require 'mugu-window)

;;; Code:
(defconst mugu-org-wconf-tab-name
  "org"
  "Name of the tab reserved for wconf buffer.")

(defun mugu-org-wconf-tab--eligible-buffer-p (buffer-name &rest _)
  "Predicate indicating if BUFFER-NAME is eligible for TAB display buffer rule."
  (and (s-equals? mugu-org-wconf-tab-name (mugu-tab-current-tab-name))
       (mugu-org-wconf-org-buffer-p buffer-name)))

(defun mugu-org-wconf-tab--activate ()
  "."
  (mugu-org-wconf-mode -1)
  (mugu-org-wconf-mode +1)
  (mugu-window-configure-normal-window #'mugu-org-wconf-tab--eligible-buffer-p)
  (save-current-tab
   (mugu-tab-new "org")
   (mugu-tab-rename mugu-org-wconf-tab-name)))

(defun mugu-org-wconf-tab--deactivate ()
  "."
  (mugu-window-remove-display-rule #'mugu-org-wconf-tab--eligible-buffer-p)
  (save-current-tab
   (mugu-tab-try-delete mugu-org-wconf-tab-name)))

(define-minor-mode mugu-org-wconf-tab-mode
  "Handle display configuration for org files when `mugu-tab' active."
  :global t
  :group 'mugu
  (if mugu-org-wconf-tab-mode
      (mugu-org-wconf-tab--activate)
    (mugu-org-wconf-tab--deactivate)))

(provide 'mugu-org-wconf-tab)
;;; mugu-org-wconf-tab ends here
