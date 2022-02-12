;;; mugu-plantuml-ext --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'plantuml-mode)

(defun mugu-plantuml-ext-export ()
  "Export as."
  (interactive)
  (when (eq major-mode 'plantuml-mode)
    (call-process-shell-command (format "%s %s" plantuml-executable-path buffer-file-name))))

(defun mugu-plantuml-ext-goto-docs ()
  "Open a navigator to docs."
  (interactive)
  (browse-url "https://plantuml.com/en/"))

(provide 'mugu-plantuml-ext)
;;; mugu-plantuml-ext ends here
