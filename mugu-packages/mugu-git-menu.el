(require 'hydra)
(require 'magit)
(require 'mugu-menu)
(require 'mugu-core)

(defhydra mugu-magit-visibility-hydra (:color red :hint nil)
  " visibility hydra"
  ("<tab>" magit-section-toggle "toggle at point" :column "Visibility - Section")
  ("C-<tab>" magit-section-cycle "cycle section")
  ("S-<tab>" magit-section-cycle-global "min/max all")
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

(defhydra mugu-magit-status-actions
  (:color red :hint nil)
  ("s" magit-stage "stage" :column "Actions (Status)")
  ("u" magit-unstage "unstage")
  ("d" magit-discard "discard")
  ("c" magit-commit-popup "commit" :color blue))

(defhydra mugu-magit-main-hydra (:color amaranth :inherit (mugu-magit-visibility-hydra/heads
                                                       mugu-magit-status-actions/heads
                                                       mugu-magit-navigation-hydra/heads))
  "
                                  -- MAGIT --
"
  ("r" magit-refresh "refresh" :column "3-Global Actions")
  ("R" magit-refresh-all "refresh all")
  ("$" magit-process-buffer "goto magit status")
  ("g" magit-visit-thing "visit" :color blue)
  ("q" nil "quit menu" :color blue :column nil)
  ("<RET>" magit-dispatch-popup "magit dispatch" :color blue)
  ("Q" magit-mode-bury-buffer "quit magit" :color blue))


(provide 'mugu-git-menu)
