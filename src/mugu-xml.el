;;; mugu-xml --- wrapper interface for xml mode -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'mugu-lang)

(defun mugu-xml-pretty-print ()
  "Pretty print current xml buffer."
  (interactive)
  (when (eq major-mode 'nxml-mode)
    (shell-command (format "xmllint --format %s" buffer-file-truename)
                   (current-buffer))))

(defalias 'mugu-xml-lang-menu #'mugu-lang-menu)

(provide 'mugu-xml)
;;; mugu-xml ends here
