;;; mugu-conf-org --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; (require 'mugu-org-hack)
(require 'mugu-misc)
(require 'use-package)
(require 'mugu-org-hack)

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
  (mugu-orgi-configure-keys))

(use-package mugu-org-workflow
  :after org
  :straight nil
  :config
  (mugu-orgw-set-configuration))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :straight (:host github :repo "jethrokuan/org-roam")
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org/roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :config
  (push 'company-org-roam company-backends))

(use-package mugu-wconf
  :straight nil
  :after mugu-org-utils
  :config (mugu-wconf-add-rule 100 (lambda (buffer)
                                     (when buffer
                                       (with-current-buffer buffer
                                         (when (eq major-mode 'org-mode) "org"))))))

(provide 'mugu-conf-org)
;;; mugu-conf-org ends here
