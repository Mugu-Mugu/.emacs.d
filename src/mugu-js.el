;;; mugu-js --- Stub to provide hook for js mode loading -*- lexical-binding: t -*-
;;; Commentary:
(require 'mugu-lsp)

;;; Code:
(defun mugu-js-configure-file ()
  "To configure js file on load.")

(defun mugu-js-configure-package ()
  "To configure package on load."
  (mugu-lsp-activate-for-keymap 'js-mode-map))

(provide 'mugu-js)
;;; mugu-js ends here
