(require 'hydra)
(require 'org)

;;; general main menu meant to be used outside or within org
(defhydra mugu-org-main-menu
  (:color blue :hint nil) 
  "
ORG MENU
^^^^^^^^-----------------------------------------------------------------------------------------------
_a_: agenda 
_l_: store link
"
  ("a" org-agenda)
  ("l" org-store-link)
  )

;;; mean meant to be used when org mode is active
(defhydra mugu-org-mode-menu
  (:color red :hint nil :idle 0.5)
  "
ORG MODE MENU
^^^^^^^^-----------------------------------------------------------------------------------------------
_a_ agenda
_l_ store link
"
  ("a" org-agenda)
  ("l" org-store-link)
  )

(provide 'mugu-org-menu)
