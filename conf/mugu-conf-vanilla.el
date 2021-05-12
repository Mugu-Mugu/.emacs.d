;;; mugu-conf-vanilla --- Summary
;; Gather various settings for vanilla emacs
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package mugu-vanilla
  :straight nil
  :config
  (mugu-vanilla-set-perf-settings)
  (mugu-vanilla-set-startup-settings)
  (mugu-vanilla-set-encoding-settings)
  (mugu-vanilla-set-backup-settings)
  (mugu-vanilla-set-visual-settings)
  (mugu-vanilla-set-other-settings)
  (mugu-vanilla-set-evil-initial-states)
  (superword-mode)
  (global-so-long-mode)
  )

(use-package diminish)

(use-package delight)

(use-package whitespace
  :delight global-whitespace-mode
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (whitespace-style '(trailing))
  :config
  (global-whitespace-mode))

(use-package paren
  :delight
  :custom
  (show-paren-style 'mixed)
  :config
  (show-paren-mode))

(use-package mugu-directory
  :straight nil
  :config
  (add-hook 'find-directory-functions #'mugu-directory-find-file-try-cd))

(use-package info
  :mode-hydra
  (Info-mode
   (:title (with-faicon "info" "Info Mode") :color pink)
   ("Navigation"
   (("j" Info-forward-node)
     ("k" Info-backward-node)
     ("n" Info-next)
     ("p" Info-prev)
     ("b" Info-top-node)
     ("u" Info-up)
     ("e" Info-final-node))
    "Reference"
    (("TAB" Info-next-reference)
     ("<s-tab>" Info-prev-reference)
     ("RET" Info-follow-nearest-node)
     ("l" ace-link)
     ("L" counsel-ace-link)
     ("a" info-apropos))
    "Goto something"
    (("g" Info-goto-node)
     ("G" Info-goto-emacs-command-node)
     ("i" Info-index)
     ("s" counsel-info-lookup-symbol))
    "Macro actions"
    (("d" Info-directory)
     ("m" Info-menu)
     ("t" Info-toc))
    "history"
    (("C-o" Info-history-back)
     ("C-i" Info-history-forward)
     ("H" Info-history))
    "Help"
    (("?" Info-summary)
     ("h" Info-help)))))

(use-package elec-pair
  :config
  (defun mugu-electric-pair-sane-inhibit(char)
    "Based upon `electric-pair-conservative-inhibit' but way saner."
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     ;; (and (eq char (char-before))
     ;;      (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)))
  :custom
  (electric-pair-inhibit-predicate 'mugu-electric-pair-sane-inhibit)
  (electric-pair-mode t)

  :config)

(provide 'mugu-conf-vanilla)
;;; mugu-conf-vanilla ends here
