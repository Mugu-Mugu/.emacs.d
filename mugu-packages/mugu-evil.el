;;; Package --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)


(use-package key-chord
  :demand
  :config (progn
            (key-chord-mode t)
            (setq key-chord-two-keys-delay 0.1)))

(use-package undo-tree
  :straight (:host gitlab :repo "mwasilewski/undo-tree"))

;;; loading package
(use-package evil
  :demand
  :diminish
  :config
  (evil-mode +1)
  (setq evil-want-C-i-jump nil))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; super escape
;; to improve
;;; pluggin mode activation
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

(use-package evil-mc
  :defer 4
  :diminish
  :config
  (global-evil-mc-mode))

(provide 'mugu-evil)
;;; mugu-evil ends here
