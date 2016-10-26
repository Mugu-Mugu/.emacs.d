(require 'hydra)
(require 'org)

;;; mean meant to be used when org mode is active
(defhydra mugu-org-mode-menu
  (:color pink :hint nil :columns 4 :inherit (mugu-org-hjkl-arrows-menu/heads))
  "
ORG MENU
^^^^^^^^-----------------------------------------------------------------------------------------------
Navigation     : [_h_/_j_/_k_/_l_] [_/_] sparse tree

Cut/Copy/Paste : [_d_/_y_/_p_]^^   [_s_] sort
Mark Elem/Tree : [_m_/_M_]  ^^^^   [_r_] refile
"
  ("k" outline-previous-visible-heading)
  ("j" outline-next-visible-heading)
  ("l" org-forward-heading-same-level)
  ("h" org-backward-heading-same-level)
  ("b" outline-up-heading)
  ("/" org-sparse-tree :color blue)
  ("y" org-copy-subtree)
  ("d" org-cut-subtree)
  ("p" org-paste-subtree)
  ("m" org-mark-element)
  ("M" org-mark-subtree)
  ("RET" org-insert-heading-respect-content)
  ("s" org-sort)
  ("r" org-refile)
  ("t" org-toggle-heading)
  ("o" org-insert-todo-heading "insert todo" :color blue )
  ("q" nil "exit" :color blue)
  )

;;; replace arrows binding by hjkl one
;;; can be used in insert mode (shift-ones are not really interesting there)
;;; is also used by another hydra
(defhydra mugu-org-hjkl-arrows-menu
  (:color pink :columns 4)
  ("C-h" org-shiftcontrolleft "next todo set")
  ("M-h" org-metaleft "demote heading")
  ("M-H" org-shiftmetaleft "demote subtree")
  ("H"   org-shiftleft "priority up")
  ("C-l" org-shiftcontrolright "prev todo set")
  ("M-l" org-metaright "promote heading")
  ("M-L" org-shiftmetaright "promote subtree")
  ("L"   org-shiftright "cycle todo/item/misc prev")
  ("C-j" org-shiftcontroldown "clock down")
  ("M-j" org-metadown "move subtree down")
  ("M-J" org-shiftmetadown "move subtree down")
  ("J"   org-shiftdown "priority down")
  ("C-k" org-shiftcontrolup "clock up")
  ("M-k" org-metaup "move subtree up")
  ("M-K" org-shiftmetaup "move subtree up")
  ("K"   org-shiftup    "cycle todo/item/misc next")
  ("C-g" nil "quit"))

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
