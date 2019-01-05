;;; Package --- Summary
;; tbc
;;; Commentary:
;;; Code:

;; fixme core function should be made available elsewhere
(require 'mugu-core)
(require 'mugu-org-hack)


(use-package org-agenda
  :straight (:local-repo)
  :defer t
  :functions mugu-org-menu/agenda-menu mugu-menu-register-mode-menu
  :after org
  :config
  (after 'mugu-org-menu
    (add-hook 'org-agenda-mode-hook #'mugu-org-menu/agenda-menu)
    (mugu-menu-register-mode-menu 'org-agenda-mode 'mugu-org-menu/agenda-menu))
  :bind
  (:map org-agenda-mode-map
        ("SPC" . mugu-menu-main-menu)))

(use-package org
  :defer t)

(use-package org-indent
  :straight (:local-repo)
  :defer t
  :after org
  :delight org-indent-mode)

(use-package mugu-org-interface
  :straight (:local-repo)
  :defer t
  :after org
  :config
  (mugu-orgi-set-configuration)
  (mugu-orgi-activate-menus))

(use-package mugu-org-workflow
  :straight (:local-repo)
  :defer t
  :after org
  :config
  (mugu-orgw-set-configuration))


;;; mugu-org ends here
(provide 'mugu-org)
