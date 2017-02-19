;;; Package --- Summary
;; tbc
;;; Commentary:

;;; Code:
(use-package org
  :ensure
  :defer t
  :defines org-agenda-custom-commands org-capture-templates
  :config
  (require 'mugu-org-utils)
  (require 'org-agenda)
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))
  (setq org-use-fast-todo-selection t)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-custom-commands
        '(("ct" "list all active actions" todo "NEXT"
           ((org-agenda-prefix-format "%i %-10:c %b")
            (org-agenda-skip-function #'mugu-org-skip-project-or-inactive-branch)))
          ("cb" "list all actions not planified" todo "TODO"
           ((org-agenda-prefix-format "%i %-10:c %b")
            (org-agenda-todo-list-sublevels nil)))
          ("ca" "dashboard" ((tags-todo "-REFILE/NEXT"
                                        ((org-agenda-overriding-header "Stuck Projects")
                                         (org-agenda-prefix-format "%i %-10:c %b")
                                         (org-agenda-skip-function #'mugu-org-skip-project-not-stuck)))
                             (tags-todo "REFILE"
                                        ((org-agenda-overriding-header "Task to refile")
                                         (org-agenda-prefix-format "%i %-10:c %b")))
                             (tags-todo "-REFILE/NEXT"
                                        ((org-agenda-overriding-header "Active Actions")
                                         (org-agenda-skip-function #'mugu-org-skip-project-or-inactive-branch)
                                         (org-agenda-prefix-format "%i %-10:c %b")))
                             (tags-todo "-REFILE/TODO"
                                        ((org-agenda-overriding-header "Backlog")
                                         (org-agenda-prefix-format "%i %-10:c %b")
                                         (org-tags-match-list-sublevels nil)
                                         (org-agenda-todo-list-sublevels nil)))))))

  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline "~/org/torefile.org" "Tasks")
           "* TODO %?\n  %i")
          ("e" "Emacs Todo" entry (file+headline ,(concat user-emacs-directory "emacs.org") "TO_REFILE")
           "* TODO %?\n  %i")
          ("n" "Note" entry (file+headline+datetree "~/org/torefile.org" "Notes")
           "* %?\nEntered on %U\n  %i")))
  (setq org-agenda-files `(,(expand-file-name "~/org")
                           ,(expand-file-name (concat user-emacs-directory "emacs.org")))))

(use-package mugu-org-menu
  :functions mugu-menu-register-mode-menu mugu-org-agenda-menu
  :commands mugu-org-main-menu/body
  :after org
  :config
  (require 'mugu-menu)
  (add-hook 'org-agenda-mode-hook #'mugu-org-agenda-menu)
  (mugu-menu-register-mode-menu 'org-mode 'mugu-org-internal-menu)
  (mugu-menu-register-mode-menu 'org-agenda-mode 'mugu-org-agenda-menu))

(use-package org-agenda
  :defer
  :bind
  (:map org-agenda-mode-map
        ("SPC" . mugu-menu-main-menu)))

(provide 'mugu-org)
;;; mugu-org ends here
