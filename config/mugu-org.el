
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
  :config
  (require 'mugu-org-utils)
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))
  (setq org-use-fast-todo-selection t)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-custom-commands
        '(("ct" "list all active actions" todo "NEXT"
          ((org-agenda-prefix-format "%i %-10:c %b")
           (org-agenda-skip-function #'mugu-org-skip-project-or-inactive-branch)))
          ("cs" "list all stuck projects" todo "NEXT" 
           ((org-agenda-prefix-format "%i %-10:c %b")
            (org-agenda-skip-function #'mugu-org-skip-project-not-stuck)))
          ("cb" "list all actions not planified" todo "TODO" 
           ((org-agenda-prefix-format "%i %-10:c %b")
            (org-agenda-todo-list-sublevels nil)))
          ))
  (customize-set-value 'org-agenda-files (file-expand-wildcards "~/org/*.org")))

(use-package mugu-org-menu
  :commands mugu-org-main-menu/body
  :after org
  :init
  :config 
  (mugu-menu-register-mode-menu 'org-mode 'mugu-org-internal-menu)
  )

(provide 'mugu-org)
