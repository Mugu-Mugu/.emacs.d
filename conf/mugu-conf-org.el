;; mugu-conf-org --- Summary
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
  :straight nil
  :defer
  :hook
  (org-mode . (lambda ()  (auto-fill-mode) (set-fill-column 110)))
  (org-capture-mode . (lambda () (evil-insert-state)))
  :custom
  (org-src-preserve-indentation t))

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
  :commands mugu-orgi-menu-global
  :config
  (mugu-orgi-set-configuration)
  (mugu-orgi-configure-keys)
  (mugu-orgi-define-ivy-actions))

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

(use-package mugu-org-workflow
  :after org
  :straight nil
  :config
  (mugu-orgw-set-configuration))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  ;; :straight (:host github :repo "jethrokuan/org-roam")
  :after org
  :demand
  :config
  (org-roam-mode)
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
  :straight nil
  :commands mugu-roam-capture-daily-note mugu-roam-capture-daily-todo-with-link mugu-roam-capture-daily-todo mugu-roam-daily-filename
  :general (:keymaps 'global
                     [remap mugu-feature-org-insert-link-note] #'mugu-roam-insert))

(use-package company-org-roam
  :disabled "it has been obsoleted and archived and now rely on company-cpaf"
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :after org-roam
  :config
  (push 'company-org-roam company-backends))

(use-package org-sql
  :after org
  :custom
  (org-sql-use-tag-inheritance t)
  (org-sql-files (-map #'file-truename org-agenda-files)))

(use-package mugu-org-sql
  :straight nil
  :defer
  :config
  (mugu-org-sql-mode -1))

(use-package org-protocol
  :defer
  :straight nil)

(use-package ox-reveal
  :after org
  :defer
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package org-protocol-capture-html
  :straight (:host github :repo "alphapapa/org-protocol-capture-html")
  :after org)

(use-package org-protocol-capture-html
  :straight (:host github :repo "alphapapa/org-protocol-capture-html")
  :defer)

(use-package org-protocol-capture-html
  :straight (:host github :repo "alphapapa/org-protocol-capture-html")
  :config
  (require 'mugu-org-protocol)
  (push `("w" "Web site"
          entry (file+headline ,mugu-orgp-capture-file-path ,mugu-orgp-capture-default-inbox)
          "* %a :website:\n\n%U %?\n\n%:initial"
          :immediate-finish t)
        org-capture-templates)
  (push '("capture-html"
          :protocol "capture-html"
          :function org-protocol-capture-html--with-pandoc)
        org-protocol-protocol-alist))

(use-package org
  :straight nil
  :after mugu-counsel
  :general
  (:keymaps '(org-mode-map) :states 'normal
            [remap mugu-feature-pop-binding-description] (mugu-counsel-generate-descbinds "org ^")))

(provide 'mugu-conf-org)
;;; mugu-conf-org ends here
