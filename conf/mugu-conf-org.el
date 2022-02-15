;; mugu-conf-org --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; (require 'mugu-org-hack)
(require 'mugu-misc)
(require 'use-package)

(use-package org-agenda
  :after org
  :straight nil)

(use-package mugu-org-hack
  :straight nil
  :after org
  :config
  (mugu-org-hack-mode))

(use-package org
  :straight nil
  :defer
  :config
  (add-to-list 'org-modules 'org-id)
  (add-to-list 'org-modules 'org-habit)
  (setq org-id-link-to-org-use-id 'use-existing)
  :general
  (:keymaps '(org-mode-map) :states '(normal motion)
            "<tab>" #'org-cycle)
  :hook
  (org-mode . (lambda () (auto-fill-mode) (set-fill-column 110)))
  (org-capture-mode . (lambda () (evil-insert-state)))
  :custom
  (org-src-preserve-indentation t)
  (org-use-fast-todo-selection t)
  (org-refile-use-outline-path t)
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-outline-path-complete-in-steps nil)
  (org-startup-indented t)
  (org-indirect-buffer-display 'current-window)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit 't)
  (org-agenda-inhibit-startup t)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-align-tags-to-column -150)
  (org-agenda-tags-column -150)
  (org-tags-column -120)
  (org-agenda-log-mode-add-notes nil)
  (org-log-reschedule nil)
  (org-use-fast-todo-selection 'expert)
  (org-habit-show-habits-only-for-today t)
  (calendar-week-start-day 1)
  (org-habit-graph-column 80)
  (org-lowest-priority ?F)
  (org-todo-keywords (quote ((sequence "TODO(t)" "ACTIVE(a)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "STOP(s@)"))))
  (org-agenda-files `(,(expand-file-name "~/org/") ,(expand-file-name "~/org/roam"))))

(use-package org-plus-contrib
  :straight nil
  :defer)

(use-package org-indent
  :after org
  :straight nil
  :delight org-indent-mode)

(use-package mugu-org-interface
  :defer
  :straight nil
  :general
  (:keymaps 'org-mode-map
            [remap mugu-menu-call-mode-menu] #'mugu-orgi-menu-org-major-mode)
  :commands mugu-orgi-menu-global)

(use-package mugu-org-interface
  :straight nil
  :demand :after org)

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (evil-org-agenda-set-keys))

(use-package mugu-org-wconf
  :straight nil
  :after org
  :config
  (mugu-org-wconf-mode +1)
  :custom
  ;; org-roam-buffer-window-parameters
  (org-roam-buffer-window-parameters '((mugu-window t)))
  (org-roam-buffer-position 'bottom)
  (org-roam-buffer-height 0.5)
  (org-roam-buffer-width 0.2))

(use-package mugu-org-wconf-tab
  :straight nil
  :after (org mugu-tab)
  :config
  (mugu-org-wconf-tab-mode +1))

(use-package mugu-org-interface
  :straight nil
  :after org-roam)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :after org
  :demand
  :config
  (org-roam-mode)
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-dailies-directory "daily/")
  (org-roam-db-update-method 'immediate)
  :general (:keymaps 'global
                     [remap mugu-feature-org-note] #'org-roam-node-find))

(use-package mugu-roam
  :after org-roam
  :demand
  :straight nil
  :commands mugu-roam-capture-daily-note mugu-roam-capture-daily-todo-with-link mugu-roam-capture-daily-todo mugu-roam-daily-filename
  :general (:keymaps 'global
                     [remap mugu-feature-org-insert-link-note] #'mugu-roam-insert))

(use-package org-protocol
  :after org
  :defer
  :straight nil)

(use-package ox-reveal
  :after org
  :defer
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package org
  :straight nil
  :after mugu-counsel
  :defer
  :general
  (:keymaps '(org-mode-map) :states 'normal
            [remap mugu-feature-pop-binding-description] (mugu-counsel-generate-descbinds "org ^")))


(use-package org-ql
  :defer
  :config
  (setq org-agenda-custom-commands
        '(("ces" "Custom: Agenda and Emacs SOMEDAY [#A] items"
           ((org-ql-block '(and
                            (priority "A")
                            (todo "TODO"))
                          ((org-ql-block-header "SOMEDAY :Emacs: High-priority")))
            )))))

(use-package org-transclusion
  :disabled "Not applicable for todo but interesting for note taking nonetheless"
  :defer)

(provide 'mugu-conf-org)
;;; mugu-conf-org ends here
