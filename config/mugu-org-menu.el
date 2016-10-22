(require 'hydra)
(require 'org)

;;; mean meant to be used when org mode is active
(defhydra mugu-org-mode-menu
  (:color red :hint nil) 
  "
ORG MENU
^^^^^^^^-----------------------------------------------------------------------------------------------
Navigation : [_l_/_h_] next/previous header same level  [_b_] go up heading
             [_j_/_k_] next/previous header 

Submenus :
"
  ("k" outline-previous-visible-heading)
  ("j" outline-next-visible-heading)
  ("l" org-forward-heading-same-level)
  ("h" org-backward-heading-same-level)
  ("b" outline-up-heading)
  ("q" nil "exit" :color blue)
  ("/" org-sparse-tree "space trees" :color blue)
  ("s" mugu-org-structured-menu "Structured editing" :color blue))


(defhydra mugu-org-structured-hydra
  (:color blue :hint nil)
  "
ORG SUBMENU  : Structured editing
^^^^^^^^-----------------------------------------------------------------------------------------------
Promoting/Demoting : [_k_/_j_] headings     [_h_/_l_] subtree                   
Copy/Paste/Move    : [_K_/_J_] move subtree [_y_] copy [_d_] delete [_p_] paste
Mark               : [_m_/_M_] element/subtree

Misc :
"
  ("j" org-do-demote)
  ("k" org-do-promote)
  ("l" org-demote-subtree)
  ("h" org-promote-subtree)
  ("K" org-move-subtree-up)
  ("J" org-move-subtree-down)
  ("y" org-copy-subtree)
  ("d" org-cut-subtree)
  ("p" org-paste-subtree)
  ("m" org-mark-element)
  ("M" org-mark-subtree)
  ("q" mugu-org-mode-menu/body "exit" :color blue)
  ("RET" org-insert-heading-respect-content)
  ("S" org-sort "sort")
  ("r" org-refile "refile")
  ("u" undo-tree-undo)
  ("t" org-toggle-heading "transform heading"))
(defalias 'mugu-org-structured-menu 'mugu-org-structured-hydra/body)

;;; general main menu meant to be used outside or within org
(defhydra mugu-org-main-menu
  (:color red :hint nil :idle 0.1)
  "
ORG MODE MENU
^^^^^^^^-----------------------------------------------------------------------------------------------
_a_ agenda
_l_ store link
"
  ("a" org-agenda)
  ("l" org-store-link)
  )

(defalias 'mugu-org-internal-menu 'mugu-org-mode-menu/body)

(provide 'mugu-org-menu)
