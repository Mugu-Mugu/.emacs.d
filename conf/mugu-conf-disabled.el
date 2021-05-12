;;; mugu-conf-disabled --- gather configuration of disabled packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Some package may be relevant in the future and thus configuration is kept

;;; Code:
(use-package org-journal
  :defer
  :disabled
  :custom
  (org-journal-dir "~/org/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-type 'weekly)
  (org-journal-file-format "%Y%m%d.org")

  :config
   (mugu-window-configure-side-window (lambda (buffer _action)
                                       (eq (buffer-local-value 'major-mode (get-buffer buffer)) 'org-journal-mode))
                                      'bottom
                                      0.6
                                      'dont-bury))

(provide 'mugu-conf-disabled)
;;; mugu-conf-disabled ends here
