;;; mugu-pyright --- Configuration for python based on pyright -*- lexical-binding: t -*-
;;; Commentary:
;; This lsp server is very fast and strong for type hinting but lacks some feature provided by
;; pyls for instance.
;; This package emulates the feature that are missing through a shared mode

;;; Code:
(require 'use-package)
(require 'blacken)
(require 'lsp-pyright)
(require 'general)

(defun isortify-cleanup (_)
  "Somehow isortify pollute the buffer.
This hack removes it."
  (let* ((original-point (point)))
    (search-forward "[0m")
    (replace-match "")
    (goto-char original-point)))


(defun mugu-pyright--activate ()
  "Setup for mugu-pyright-mode."
  (blacken-mode 1)
  (general-define-key :keymaps 'mugu-pyright-mode-map
                      [remap mugu-lang-format-buffer] #'blacken-buffer)
  (add-to-list 'minor-mode-overriding-map-alist
               (cons 'mugu-pyright-mode-map mugu-pyright-mode-map)))

(defun mugu-pyright--deactivate ()
  "Tear down for mugu-pyright-mode."
  (blacken-mode -1)
  (advice-remove 'isortify-buffer #'isortify-cleanup))

(define-minor-mode mugu-pyright-mode
  "Define mugu-pyright-mode."
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-pyright-mode
      (mugu-pyright--activate)
    (mugu-pyright--deactivate)))


(provide 'mugu-pyright)
;;; mugu-pyright ends here
