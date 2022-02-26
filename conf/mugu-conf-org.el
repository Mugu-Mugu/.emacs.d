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
  (org-refile-use-cache t)
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
  (org-default-priority ?F)
  (org-archive-location "~/org/archives/%s_archive::* From %s")
  (org-todo-keywords (quote ((sequence "TODO(t)" "ACTIVE(a)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "STOP(s@)"))))
  (org-agenda-files `(,(expand-file-name "~/org/legacy") ,(expand-file-name "~/org/roam")))
  (org-refile-targets '((org-agenda-files :tag . "inbox"))))

(use-package evil-collection
  :after org-agenda
  :custom
  (evil-collection-calendar-want-org-bindings t))

(use-package org-plus-contrib
  :straight nil
  :defer)

(use-package org-indent
  :after org
  :straight nil
  :delight org-indent-mode)

(use-package mugu-org-interface
  :straight nil
  :defer
  :general
  (:keymaps 'org-mode-map
            [remap mugu-menu-call-mode-menu] #'mugu-orgi-menu-org-major-mode)
  (:keymaps 'global [remap mugu-feature-org-insert-link-note] #'mugu-orgi-roam-insert-node
            )
  :commands mugu-orgi-menu-global)

(use-package mugu-org-interface
  :straight nil
  :demand
  :after org)

(use-package evil-org
  :after org
  :functions (evil-org-set-key-theme evil-org-agenda-set-keys)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :custom
  (evil-org-use-additional-insert t))

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
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+setupfile: ~/org/tags.org"))))
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+setupfile: ~/org/tags.org")
      :unnarrowed t)))
  :general (:keymaps 'global
                     [remap mugu-feature-org-note] #'org-roam-node-find))

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
  :after org
  :config
  ;; to fix a binding issue on headers for org-ql views
  (setq org-super-agenda-header-map (make-sparse-keymap))
  :custom
  (org-ql-ask-unsafe-queries nil))

(use-package mugu-org-workflow
  :straight nil
  :after org
  :general
  ([remap mugu-feature-org-view-active-tasks] #'mugu-orgw-view-active-tasks
   [remap mugu-feature-org-goto-planification-note] #'mugu-orgw-goto-planification-note
   [remap mugu-feature-org-goto-setupfile] #'mugu-orgw-goto-setupfile))

(use-package org-super-agenda
  :after mugu-counsel
  :defer
  :general
  (:keymaps 'org-agenda-mode-map :states 'normal
            [remap mugu-feature-pop-binding-description] (mugu-counsel-generate-descbinds "org ^")))

(use-package org-transclusion
  :disabled "Not applicable for todo but interesting for note taking nonetheless"
  :defer)

(provide 'mugu-conf-org)
;;; mugu-conf-org.el ends here
