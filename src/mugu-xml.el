;;; mugu-xml --- wrapper interface for xml mode -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-menu)

(defun mugu-xml-pretty-print ()
  "Pretty print current xml buffer."
  (interactive)
  (when (eq major-mode 'nxml-mode)
    (shell-command (format "xmllint --format %s" buffer-file-truename)
                   (current-buffer))))

(defmenu mugu-xml-menu (:hint nil :color blue)
  ("f" mugu-xml-pretty-print "format xml file" :column "Action"))


(defun mugu-xml-activate ()
  "Activate xml integration."
  (mugu-menu-register-mode-menu 'nxml-mode #'mugu-xml-menu)
  (remove-hook 'nxml-mode-hook #'mugu-xml-activate))

(provide 'mugu-xml)
;;; mugu-xml ends here
