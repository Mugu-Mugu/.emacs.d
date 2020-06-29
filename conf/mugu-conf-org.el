;;; mugu-conf-org --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; (require 'mugu-org-hack)
(require 'mugu-misc)
(require 'use-package)

(use-package org-agenda
  :after org
  :straight nil
  :general
  (:keymaps 'org-agenda-mode-map
           [remap mugu-menu-call-mode-menu] #'mugu-orgi-menu-agenda-major-mode)
  :hook (org-agenda-mode . mugu-orgi-menu-agenda-major-mode)
  :bind
  (:map org-agenda-mode-map
        ("SPC" . mugu-menu-main-menu)))

(use-package mugu-org-hack
  :straight nil
  :after org
  :config
  (mugu-org-hack-mode))

(use-package org
  :defer
  :interpreter "org")

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
  :commands mugu-orgi-menu-global
  :config
  (mugu-orgi-set-configuration)
  (mugu-orgi-configure-keys)
  (mugu-orgi-define-ivy-actions))

(use-package mugu-org-interface
  :straight nil
  :after org)

(use-package mugu-org-workflow
  :after org
  :straight nil
  :config
  (mugu-orgw-set-configuration))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :straight (:host github :repo "jethrokuan/org-roam")
  :after org
  :config
  (org-roam-mode)
  (require 'org-roam-protocol)
  :custom (org-roam-directory "~/org/roam")
  :general (:keymaps 'global
                     [remap mugu-menu-org-note] #'org-roam-find-file))

(use-package mugu-roam
  :straight nil
  :commands mugu-roam-capture-daily-note mugu-roam-capture-daily-todo-with-link mugu-roam-capture-daily-todo mugu-roam-daily-filename
  :general (:keymaps 'global
                     [remap mugu-menu-org-insert-link-note] #'mugu-roam-insert))

(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :after org-roam
  :config
  (push 'company-org-roam company-backends))

(use-package org-sql
  :after org
  :custom
  (org-sql-use-tag-inheritance t)
  (org-sql-files org-agenda-files))

(use-package mugu-org-sql
  :straight nil
  :defer
  :config
  (mugu-org-sql-mode))

(use-package mugu-wconf
  :straight nil
  :after mugu-org-utils
  :config
  (add-to-list 'display-buffer-alist '(".*org" (display-buffer-same-window
                                                display-buffer-reuse-window
                                                display-buffer-reuse-mode-window
                                                display-buffer-use-some-window)))
  (mugu-wconf-add-rule 100 (lambda (buffer)
                             (when buffer
                               (with-current-buffer buffer
                                 (when (eq major-mode 'org-mode) "org"))))))

(use-package org-journal
  :after org
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  :config
  (mugu-window-configure-side-window (lambda (buffer _action)
                                       (eq (buffer-local-value 'major-mode (get-buffer buffer)) 'org-journal-mode))
                                     'bottom 0.3))

(use-package ox-reveal
  :after org
  :defer
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(provide 'mugu-conf-org)
;;; mugu-conf-org ends here
