;;; Package --- Summary
;; Provide base hydra bindings for org mode as well as means to dynamically bind more.
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'mugu-hydra)
(require 'org)
(require 'org-agenda)
(require 'mugu-menu)

(defmenu mugu-org-menu/agenda (:color amaranth :hint nil)
  "Mugu"
  ("l" mugu-org-utils/agenda-forward-block "next line" :column "1-Navigation")
  ("h" mugu-org-utils/agenda-backward-block "previous line")
  ("j" org-agenda-next-item "next item")
  ("k" org-agenda-previous-item "previous item")
  ("J" org-agenda-drag-line-forward "drag up")
  ("K" org-agenda-drag-line-backward "drag bot")
  ("r" org-agenda-refile "refile" :column "2a-Task Actions")
  ("d" org-agenda-kill "kill")
  (":" org-agenda-set-tags "set tags" :column "2b-Task Data")
  ("n" org-agenda-add-note "add note")
  ("t" org-agenda-todo "set todos")
  ("L" org-agenda-priority-up "increase priority")
  ("H" org-agenda-priority-down "decrease priority")
  ("a" org-attach "attach")
  ("p" org-agenda-set-property "set property")
  ("e" org-agenda-set-effort "set effort")
  ("m" org-agenda-bulk-toggle "mark toogle" :column "3-Mark")
  ("M" org-agenda-bulk-toggle-all "mark toogle all")
  ("ù" org-agenda-bulk-mark-regexp "mark by regexp")
  (" " nil "")
  (" " nil "Modes")
  (" " nil "------")
  ("<tab>" org-agenda-entry-text-mode "toogle detail mode")
  ("f" org-agenda-follow-mode "toogle follow mode")
  ("R" org-agenda-redo "refresh view" :column "4-GlobalAction")
  ("W" org-agenda-write "save this agenda")
  ("w" org-save-all-org-buffers "save all org files")
  ("A" org-agenda-append-agenda "append another agenda view")
  ("-" org-agenda-filter-remove-all "filter clear" :column "5-Filtering")
  ("/" org-agenda-filter-by-tag "filter by tag")
  ("!" org-agenda-filter-by-tag-refine "filter by tag (refined")
  ("=" org-agenda-filter-by-regexp "filter by regexp")
  ("<" org-agenda-filter-by-category "fitler by category")
  ("*" org-agenda-filter-by-top-headline "filter by top headline")
  ("g" org-agenda-goto "goto" :color blue :column "6-Terminating")
  ("RET" org-agenda-goto "goto" :color blue)
  ("q" org-agenda-quit "quit" :color blue)
  ("Q" org-agenda-Quit "quit" :color blue)
  ("x" org-agenda-exit "quit discard all change" :color blue)
  ("C-g" nil "exit this menu" :color blue))

;;; replace arrows binding by hjkl one
;;; can be used in insert mode (shift-ones are not really interesting there)
;;; is also used by another hydra
(defmenu  mugu-org-menu/hjkl (:color pink :hint nil)
  "HJKL bindings for ORG mode"
  ("h" org-backward-element "↑ element" :column "Navigation")
  ("l" org-forward-element "↓ element")
  ("j" org-forward-paragraph "↓ paragraph")
  ("k" org-backward-paragraph "↑ paragraph")
  ("b" org-up-element "↖ element")
  ("M-L" org-shiftmetaright "promote subtree" :column "Hierarchy")
  ("M-H" org-shiftmetaleft "demote subtree")
  (" " nil " ")
  ("M-l" org-metaright "promote heading")
  ("M-h" org-metaleft "demote heading")
  ("M-k" org-metaup   "↑ tree" :column "Reorder")
  ("M-j" org-metadown "↓ tree")
  (" " nil " ")
  ("C-k" org-shiftmetaup "↑ tree or table")
  ("C-j" org-shiftmetadown "↓ tree or table")
  ("J" org-shiftdown "↓ priority" :column "Priority")
  ("K" org-shiftup "↑  priority" :column "Priority")

  ("C-l" org-shiftcontrolright "→ change todo sequence" :column "Todo")
  ("C-h" org-shiftcontrolleft "← change todo sequence")
  (" " nil " ")
  ("H" org-shiftleft "→ cycle todo/item/misc")
  ("L" org-shiftright "← cycle todo/item/misc")
  ("C-g" nil "quit" :column "Terminate")
  ("<tab>" org-cycle "" :color red :column nil))

(defmenu mugu-org-menu/org-tree-tools
  (:color red :hint nil :inherit (mugu-org-menu-hjkl-hydra/heads))
  "bindings for advanced tree manipulation"
  ("m" org-mark-element "✔ mark" :column "Marking")
  ("M" org-mark-subtree "✔✔ mark all")
  ("y" org-copy-subtree "copy subtree" :column "Copy")
  ("d" org-cut-subtree "cut subtree")
  ("p" org-paste-subtree "paste subtree")
  ("t" org-toggle-heading "morph headline/list" :column "Transform")
  ("s" org-sort "sort subtree")
  ("/" org-sparse-tree "search" :color blue :column "Search")
  ("q" mugu-org-menu/org-menu "Return to org menu" :color blue :column nil))

(defmenu mugu-org-menu/org
  (:color blue :hint nil :inherit (mugu-org-menu-hjkl-hydra/heads))
  "bindings for ORG mode"
  ("s" org-schedule "schedule" :column "Timing")
  ("z" org-time-stamp "insert timestamp")
  ("d" org-deadline "set deadline")
  ("r" org-refile "refile subtree" :column "Misc")
  ("t" org-todo "change TODO status")
  ("c" org-ctrl-c-ctrl-c "ctrl-c²")
  ("e" mugu-org-menu/org-tree-tools-menu "expert mode")
  ("f" org-fill-paragraph "fill paragraph")
  ("a" org-attach "attach interface" :column "Insert")
  ("l" org-insert-link "insert link")
  ("n" org-add-note "insert note")
  ("u" org-set-tags-command "update tags")
  ("RET" org-insert-heading "insert" :column "Terminate")
  ("o" org-insert-todo-heading "insert todo")
  ("q" nil "exit"))

(defmenu mugu-org-menu/main (:color blue :hint nil)
  "Org mode external interface"
  ("a" org-agenda "agenda gateway" :column "Agenda")
  ("oo" (org-agenda nil "ca") "agenda overview")
  ("l" org-store-link "store link" :column "Others")
  ("c" org-capture "capture"))

(defun mugu-org-menu-register-agenda (head-char agenda-files docstring)
 "Register in org menu shortcut HEAD-CHAR to agenda overview for AGENDA-FILES.
The real shortcut will be o + HEAD-CHAR.  HEAD-CHAR may be any char but should
not be 'o' although this is not enforced.  Duplicates head are not checked
either.
DOCSTRING will be used to describe the head."
 (mugu-menu-add-entries 'mugu-org-menu/main
                      `(,(concat "o" head-char)
                        (lambda ()
                          (interactive)
                          (let ((org-agenda-files ',agenda-files))
                            (org-agenda nil "ca")))
                        ,docstring
                        :column "Agenda")))

(defun mugu-org-menu/add-head-to-main (head)
  "Register HEAD to the main org menu."
  (hydra--head-set-property head :column "Others")
  (mugu-menu-add-entries 'mugu-org-menu/main head))

(mugu-org-menu-register-agenda "e"
                               (file-expand-wildcards "~/.emacs.d/*.org")
                               "emacs tasks overview")

(provide 'mugu-org-menu)
;;; mugu-org-menu ends here
