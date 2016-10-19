
;(defvar autosave-dir "user-ema")
;(setq org-agenda-custom-commands
;      '(("cx" "TODOs sorted by state, priority, effort"
;         tags-todo "emacs+TODO=\"TODO\""
;         ((org-agenda-overriding-header "\n Emacs backlog")
;          (org-tags-match-list-sublevels 'indented)
;          (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))))))
;(setq org-stuck-projects
;      '("+PROJECT" ("TODO" "ACTIVE") () ))
;(setq org-tags-exclude-from-inheritance '("PROJECT"))
;(setq org-capture-templates
;      '(
;        ("e" "emacs" entry (file+headline 
;        ("v" "emacs" entry (file+headline 
;        ("t" "emacs" entry (file+headline 
;        )
;      )

(use-package org
  :ensure
  :defer t
  )

(use-package mugu-org-menu
  :commands mugu-org-main-menu/body
  :after org
  :config
  (after 'mugu-hydra (mugu-hydra-register-mode-hook 'org-mode-hook 'mugu-org-mode-menu)))

(provide 'mugu-org)
