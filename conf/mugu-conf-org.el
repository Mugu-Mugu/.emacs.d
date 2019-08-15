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
  :functions mugu-orgi-menu-agenda-major-mode mugu-menu-register-mode-menu
  :config
  (add-hook 'org-agenda-mode-hook #'mugu-orgi-menu-agenda-major-mode)
  (mugu-menu-register-mode-menu 'org-agenda-mode #'mugu-orgi-menu-agenda-major-mode)
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
  :commands mugu-orgi-menu-global
  :config
  (mugu-orgi-set-configuration)
  (mugu-orgi-activate-menus)
  (mugu-orgi-configure-keys))

(use-package mugu-org-workflow
  :after org
  :straight nil
  :config
  (mugu-orgw-set-configuration))

(provide 'mugu-conf-org)
;;; mugu-conf-org ends here
