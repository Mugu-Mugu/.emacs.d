;;; mugu-magit --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'magit)
(require 'mugu-menu)

(defhydra mugu-magit-visibility-hydra (:color red :hint nil)
  " visibility hydra"
  ("<tab>" magit-section-toggle "toggle at point" :column "Visibility - Section")
  ("<C-tab>" magit-section-cycle "cycle section (recursive)")
  ("<backtab>" magit-section-cycle-global "min/max all")
  ("œ" magit-section-cycle-diffs "min/max all diff")
  ("(M-)1-4" help "Set (All/Section) visibility level") ;fake head
  ("&" magit-section-show-level-1 nil)
  ("é" magit-section-show-level-2 nil)
  ("\'" magit-section-show-level-3 nil)
  ("\"" magit-section-show-level-4 nil)
  ("1" magit-section-show-level-1-all nil)
  ("2" magit-section-show-level-2-all nil)
  ("3" magit-section-show-level-3-all nil)
  ("4" magit-section-show-level-4-all nil)
  ("+" magit-diff-more-context "+ diff context" :column "Visibility - Diff Context")
  ("-" magit-diff-less-context "- diff context")
  ("0" magit-diff-default-context "default diff context"))

(defhydra mugu-magit-navigation-hydra (:color red :hint nil)
  ("b" magit-section-up "↖ section" :column "Navigation")
  ("j" magit-section-forward "↓ section")
  ("k" magit-section-backward "↑ section")
  ("J" avy-goto-line-below "↓ jump")
  ("K" avy-goto-line-above "↑ jump")
  ("v" evil-visual-char "visual" :color blue)
  ("m" evil-motion-state "free motion" :color blue))

(defhydra mugu-magit-status-actions-hydra
  (:color red :hint nil)
  ("s" magit-stage "stage" :column "Actions (Status)")
  ("u" magit-unstage "unstage")
  ("d" magit-discard "discard")
  ("e" magit-ediff-popup "ediff" :color blue)
  ("c" magit-commit-popup "commit" :color blue))

(defhydra mugu-magit-common-actions-hydra
  (:color red :hint nil)
  ("g" magit-visit-thing "visit" :column  "Actions (Global)" :color blue)
  ("r" magit-refresh "refresh")
  ("R" magit-refresh-all "refresh all")
  ("$" magit-process-buffer "goto magit status")
  ("<RET>" magit-dispatch-popup "magit dispatch" :color blue)
  ("q" nil "quit menu" :color blue :column nil)
  ("Q" magit-mode-bury-buffer "quit magit" :color blue))

(defhydra mugu-magit-default-menu
  (:color amaranth :inherit (mugu-magit-visibility-hydra/heads
                             mugu-magit-common-actions-hydra/heads
                             mugu-magit-navigation-hydra/heads))
  "
                               -- MAGIT --
")

(defhydra mugu-magit-status-menu
  (:color amaranth :inherit (mugu-magit-default-menu/heads
                             mugu-magit-status-actions-hydra/heads
                             ))
  "
                               -- MAGIT STATUS --
")

(defun mugu-magit-register-menu-mode (the-magit-mode the-menu)
  "Register for THE-MAGIT-MODE THE-MENU.
That is: bind SPC SPC for the mode and autoload the menu on first buffer entering"
  (mugu-menu-register-mode-menu `,the-magit-mode `,the-menu)
  (let ((the-hook (intern-soft (concat (symbol-name the-magit-mode) "-hook"))))
    (add-hook `,the-hook `,the-menu)))

(defun mugu-magit-enable-menus ()
  "Activate autoloding of magit menu."
  (mugu-magit-register-menu-mode 'magit-revision-mode 'mugu-magit-default-menu/body)
  (mugu-magit-register-menu-mode 'magit-log-mode 'mugu-magit-default-menu/body)
  (mugu-magit-register-menu-mode 'magit-status-mode 'mugu-magit-status-menu/body)
  (mugu-magit-register-menu-mode 'magit-diff-mode 'mugu-magit-status-menu/body))

(defun mugu-magit-configure ()
  "Set various magit settings."
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1))

(provide 'mugu-magit)
;;; mugu-magit ends here
