
(use-package magit
  :defer
  :config
  (after 'evil
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-define-key 'normal magit-status-mode-map 
     "^"    'magit-section-up
     "n"    'magit-section-forward
     "p"    'magit-section-backward
     "\M-n" 'magit-section-forward-sibling
     "\M-p" 'magit-section-backward-sibling
     "+"    'magit-diff-more-context
     "-"    'magit-diff-less-context
     "0"    'magit-diff-default-context
     "&"    'magit-section-show-level-1
     "é"    'magit-section-show-level-2
     "253"  'magit-section-show-level-3
     "'"    'magit-section-show-level-4
     "1"    'magit-section-show-level-1-all
     "2"    'magit-section-show-level-2-all
     "3"    'magit-section-show-level-3-all
     "4"    'magit-section-show-level-4-all
    "\t"    'magit-section-toggle
    [C-tab] 'magit-section-cycle
    [M-tab] 'magit-section-cycle-diffs
    [S-tab] 'magit-section-cycle-global
    "\r"    'magit-visit-thing
    [C-return] 'magit-visit-thing
     "q"    'magit-mode-bury-buffer)) 
  (after 'mugu-menu
    ;(mugu-hydra-register-mode-hook 'magit-status-mode-hook 'magit-dispatch-popup)
    ;(mugu-hydra-register-mode-hook 'magit-log-mode-hook 'magit-dispatch-popup)
    (mugu-menu-register-mode-menu 'magit-status-mode 'magit-dispatch-popup)
    ;(key-chord-define magit-status-mode-map "jk" 'evil-normal-state)
    ;(key-chord-define magit-log-mode-map "jk" 'evil-normal-state)
    )
  :ensure
  )

(provide 'mugu-git)
