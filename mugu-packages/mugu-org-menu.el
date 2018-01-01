;;; Package --- Summary
;; Provide base hydra bindings for org mode as well as means to dynamically bind more.
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'org)
(require 'org-agenda)
(require 'mugu-hydra)

(defhydra mugu-org-agenda-hydra (:color amaranth :hint nil)
  "Mugu"
  ("l" org-agenda-next-line "next line" :column "1-Navigation")
  ("h" org-agenda-previous-line "previous line")
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
(defhydra mugu-org-hjkl-hydra (:color pink :hint nil)
  "HJKL bindings for ORG mode"
  ("M-L" org-shiftmetaright "promote subtree" :column "Structured - Hierarchy")
  ("M-H" org-shiftmetaleft "demote subtree")
  (" " nil " ")
  ("M-l" org-metaright "promote heading")
  ("M-h" org-metaleft "demote heading")
  ("M-k" org-metaup   "↑ tree" :column "Structured - place")
  ("M-j" org-metadown "↓ tree")
  (" " nil " ")
  ("M-K" org-shiftmetaup "↑ tree or table")
  ("M-J" org-shiftmetadown "↓ tree or table")
  ("C-k" org-shiftcontrolup "↑ clock" :column "Attributes")
  ("C-j" org-shiftcontroldown "↓ clock")
  (" " nil " ")
  ("J" org-shiftdown "↓ priority")
  ("K" org-shiftup "↑ priority")
  ("C-l" org-shiftcontrolright "→ change todo sequence" :column "Todo")
  ("C-h" org-shiftcontrolleft "← change todo sequence")
  (" " nil " ")
  ("H" org-shiftleft "→ cycle todo/item/misc")
  ("L" org-shiftright "← cycle todo/item/misc")
  ("C-g" nil "quit" :column "Terminate"))


(defhydra mugu-org-cmd-hydra
  (:color pink :hint nil :inherit (mugu-org-hjkl-hydra/heads))
  "bindings for ORG mode"
  ("k" outline-previous-visible-heading "↑ headline" :column "Navigation")
  ("j" outline-next-visible-heading "↓ headline")
  ("l" org-forward-heading-same-level "↓ headline same level")
  ("h" org-backward-heading-same-level "↑ headline same level")
  ("b" outline-up-heading "↖ parent headline")
  ("m" org-mark-element "✔ mark" :column "Copy")
  ("M" org-mark-subtree "✔✔ mark all")
  (" " nil "")
  ("y" org-copy-subtree "copy subtree")
  ("d" org-cut-subtree "subtree  ")
  ("p" org-paste-subtree "paste subtree")
  (" " nil "")
  ("r" org-refile "refile subtree" :column "Misc")
  ("t" org-toggle-heading "morph headline/list")
  ("s" org-sort "sort subtree")
  (" " nil "")
  ("/" org-sparse-tree "search" :color blue)
  ("RET" org-insert-heading-respect-content "insert" :color blue :column "Terminate")
  ("o" org-insert-todo-heading "insert todo" :color blue)
  ("q" nil "exit" :color blue))

;;; general main menu meant to be used outside or within org
;;;###autoload
(defhydra mugu-org-main-menu (:color blue :hint nil)
  "Org mode external interface"
  ("a" org-agenda "agenda gateway" :column "Agenda")
  ("oo" (org-agenda nil "ca") "agenda overview")
  ("l" org-store-link "store link" :column "Others")
  ("c" org-capture "capture"))

(defun mugu-org-menu-register-agenda (head-char agenda-files docstring)
 "Register in org menu a shortcut HEAD-CHAR to agenda overview for AGENDA-FILES.
The real shortcut will be o + HEAD-CHAR.  HEAD-CHAR may be any char but should
not be 'o' although this is not enforced.  Duplicates head are not checked
either.
DOCSTRING will be used to describe the head."
 (mugu-hydra-add-head 'mugu-org-main-menu
                      `(,(concat "o" head-char)
                        (lambda ()
                          (interactive)
                          (let ((org-agenda-files ',agenda-files))
                            (org-agenda nil "ca")))
                        ,docstring
                        :column "Agenda")))

(defun mugu-orgm/add-head-to-main (head)
  "Register HEAD to the main org menu."
  (hydra--head-set-property head :column "Others")
  (mugu-hydra-add-head 'mugu-org-main-menu head))

(mugu-org-menu-register-agenda "e"
                               (file-expand-wildcards "~/.emacs.d/*.org")
                               "emacs tasks overview")

(defalias 'mugu-org-internal-menu 'mugu-org-cmd-hydra/body)
(defalias 'mugu-org-agenda-menu 'mugu-org-agenda-hydra/body)
(defalias 'mugu-org-hjkl-menu 'mugu-org-hjkl-hydra/body)

(provide 'mugu-org-menu)
;;; mugu-org-menu ends here
