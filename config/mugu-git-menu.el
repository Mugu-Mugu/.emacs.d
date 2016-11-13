(require 'hydra)
(require 'magit)
(require 'mugu-menu)
(require 'mugu-core)

(defhydra mugu-magit-visual-hydra
  (:color amaranth :body-pre (set-mark-command nil))
  ("j" next-line "↓ line")
  ("k" previous-line "↑ line")
  ("s" avy-goto-line "↓ line")
  ("J" avy-goto-line-below "↓ line")
  ("K" avy-goto-line-above "↑ line")
  ("v" set-mark-command "↑ line")
  ("q" hydra-pop "quit menu" :color blue :column nil)
  ("<RET>" hydra-pop "" :color blue))

(defhydra mugu-magit-visibility-hydra (:color red :hint nil)
  " :hint nilvisibility hydra"
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

(defhydra mugu-magit-main-hydra (:color pink :inherit (mugu-magit-visibility-hydra/heads))
  "
                                  -- MAGIT --
"
  ("b" magit-section-up "↖ section" :column "2-Navigation")
  ("j" magit-section-forward "↓ section")
  ("k" magit-section-backward "↑ section")
  ("l" magit-section-forward-sibling "↓ section")
  ("h" magit-section-backward-sibling "↑ section")
  ("g" magit-refresh "refresh" :column "3-Global Actions")
  ("v" (progn
         (hydra-push 'mugu-magit-main-hydra/body)
         (mugu-magit-visual-hydra/body)) "visual" :color blue "visual mode")
  ("$" magit-process-buffer "goto magit status")
  ("G" magit-refresh-all "refresh all")
  ("s" magit-stage "stage" :column "1-Actions")
  ("u" magit-unstage "unstage" :column "1-Actions")
  ("d" magit-discard "discard" :column "1-Actions")
  ("q" nil "quit menu" :color blue :column nil)
  ("<RET>" magit-dispatch-popup "magit dispatch" :color blue)
  ("Q" magit-mode-bury-buffer "quit magit" :color blue))

(after 'mugu-menu (mugu-menu-register-mode-menu 'magit-status-mode 'mugu-magit-main-hydra/body))
(add-hook 'magit-status-mode-hook 'mugu-magit-main-hydra/body)

(provide 'mugu-git-menu)
