;;; mugu-conf-cosmetics --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

;; * begin:
(use-package all-the-icons :defer
  :config
  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-material (icon str &optional height v-adjust)
    (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-mode-icon (mode str &optional height nospace face)
    (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) 0.0 0.05))
           (args `(:height ,(or height 1) :v-adjust ,v-adjust))
           (_ (when face
                (lax-plist-put args :face face)))
           (icon (apply #'all-the-icons-icon-for-mode mode args))
           (icon (if (symbolp icon)
                     (apply #'all-the-icons-octicon "file-text" args)
                   icon)))
      (s-concat icon (if nospace "" " ") str))))

(use-package font-lock+ :defer :disabled)

(use-package beacon
  ;; :defer 3
  :delight
  :disabled "lagging"
  :custom
  (beacon-color "darkorange")
  :config
  (beacon-mode))

(use-package minimap
  :disabled "not bad but does not support folds"
  :defer 3
  :delight
  :config
  (minimap-mode))

(use-package display-line-numbers
  ;; :defer 3
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type t)
  :config
  (global-display-line-numbers-mode))

(use-package dimmer
  ;; :defer 3
  :disabled "too much flashing, need some exclusion rules to be usable"
  :custom
  (dimmer-fraction 0.35)
  :config
  (dimmer-mode))

(use-package mugu-cosmetics
  :straight nil
  :config
  (mugu-cosmetics-activate))

(use-package selectric-mode
  ;; very nice and fun but bugged :(
  :disabled
  :delight selectric-mode
  :config
  (selectric-mode))

(use-package smooth-scrolling
  :disabled "doesnt do anything?"
  :defer 4
  :config
  (smooth-scrolling-mode))

(use-package sublimity
  :disabled "useless?"
  :hook prog-mode
  :config
  (require 'sublimity-scroll))

(use-package rainbow-delimiters
  :disabled "not really usefull and probably a problem for long or deeply nested file"
  :hook (prog-mode . rainbow-delimiters-mode)
  :config)

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

(use-package mugu-hydra-posframe
  :straight nil
  :after (hydra posframe)
  :demand
  :config
  (mugu-hydra-posframe-mode))

(use-package visual-fill-column
  ;; Allow for saner non disruptive long line display by virtual wrapping
  :defer)

(provide 'mugu-conf-cosmetics)
;;; mugu-conf-cosmetics ends here
