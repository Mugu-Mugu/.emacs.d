;;; Package --- Summary
;; tbc
;;; Commentary:
;;; Code:

(use-package mugu-org-menu
  :commands mugu-org-menu/main-menu
  :functions mugu-menu-register-mode-menu
  :requires mugu-menu
  :defer t
  :config
  (mugu-menu-register-mode-menu 'org-mode 'mugu-org-internal-menu))

(use-package org-agenda
  :defer
  :functions mugu-org-menu/agenda-menu mugu-menu-register-mode-menu
  :after org
  :config
  (after 'mugu-org-menu
    (add-hook 'org-agenda-mode-hook #'mugu-org-menu/agenda-menu)
    (mugu-menu-register-mode-menu 'org-agenda-mode 'mugu-org--menu))
  :bind
  (:map org-agenda-mode-map
        ("SPC" . mugu-menu-main-menu)))

(use-package org
  :ensure
  :defer t
  :functions mugu-org-menu/add-head-to-main
  :defines org-agenda-custom-commands org-capture-templates mugu-org-menu/add-head-to-main
  :config
  (require 'org-agenda)
  (require 'mugu-org-utils)

  (setq org-todo-keywords (quote ((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "SLEEPING(s)" "|" "DONE(d)" "CANCELLED(c)"))))
  (setq org-use-fast-todo-selection t)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-custom-commands
        '(("ct" "list all active actions" todo "PROGRESS"
           ((org-agenda-prefix-format "%i %-10:c %b")))
          ("cb" "list all actions not planified" todo "TODO"
           ((org-agenda-prefix-format "%i %-10:c %b")
            (org-agenda-todo-list-sublevels nil)))
          ("ca" "dashboard" ((todo "PROGRESS"
                                   ((org-agenda-overriding-header "Task in progress")
                                    (org-agenda-prefix-format "%i %-10:c %b")))
                             (tags-todo "REFILE"
                                        ((org-agenda-overriding-header "Task to refile")
                                         (org-agenda-prefix-format "%i %-10:c %b")))
                             (tags-todo "-REFILE/NEXT"
                                        ((org-agenda-overriding-header "Active Actions")
                                         (org-agenda-prefix-format "%i %-10:c %b")))))))

  (setq org-capture-templates
        `(("i" "Immediat" entry (file+headline "~/org/immediat.org" "Immediat")
           "* PROGRESS %?\n  %i")
          ("t" "Todo" entry (file+headline "~/org/torefile.org" "Tasks")
           "* TODO %?\n  %i")
          ("e" "Emacs Todo" entry (file+headline ,(concat user-emacs-directory "emacs.org") "TO_REFILE")
           "* TODO %?\n  %i")
          ("n" "Note" entry (file+headline+datetree "~/org/torefile.org" "Notes")
           "* %?\nEntered on %U\n  %i")))
  (setq org-agenda-files `(,(expand-file-name "~/org")
                           ,(expand-file-name (concat user-emacs-directory "emacs.org"))))

  (defun mugu-org/goto-progress-task ()
    "Goto any headline with PROGRESS status."
    (interactive)
    (mugu-org-utils/query-headline #'mugu-org-utils/goto-headline "/PROGRESS"))

  (defun mugu-org/refile-task ()
    "Goto any headline with REFILE tag."
    (interactive)
    (mugu-org-utils/query-headline #'mugu-org-utils/refile-headline "REFILE/TODO=*"))

  (after 'mugu-org-menu
    (mugu-org-menu/add-head-to-main '("r" mugu-org/refile-task "refile tasks" :color blue))
    (mugu-org-menu/add-head-to-main '("p" mugu-org/goto-progress-task "goto in progress task" :color blue))))

(provide 'mugu-org)
;;; mugu-org ends here
