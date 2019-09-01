;;; mugu-python --- Stub to provide hook for python mode loading -*- lexical-binding: t -*-
;;; Commentary:
(require 'mugu-lsp)

;;; Code:
(defun mugu-python-configure-file ()
  "To configure python file on load.")

(defun mugu-python-configure-package ()
  "To configure package on load."
  (mugu-lsp-activate-for-keymap 'python-mode-map))

(provide 'mugu-python)
;;; mugu-python ends here
