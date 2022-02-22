;; mugu-org-interface --- Front end to my org configuration  -*- lexical-binding: t -*-
;; tbc
;;; Commentary:
;;; Code:

(require 'mugu-menu)
(require 'mugu-feature)
(require 'ivy)
(require 'mugu-misc)
(require 'evil)
(require 'f)

;; variables
(define-mugu-feature org-note)
(define-mugu-feature org-insert-note-link)

(define-mugu-feature org-view-active-tasks "tasks currently active")
(define-mugu-feature org-view-planned-tasks "tasks with a time component (scheduled or deadline)")
(define-mugu-feature org-view-backlog-tasks "tasks active or ready to be started and priorized")
(define-mugu-feature org-view-icebox-tasks "tasks not ready and requiring evaluation")

;;; Actions

(defsubst mugu-orgi-goto-agenda-file ()
  "."
  (interactive)
  (mugu-orgi-counsel-agenda-files #'find-file))

(defun mugu-orgi-insert-checkbox ()
  "Insert a checkbox."
  (interactive)
  (newline-without-break-of-line)
  (insert "- [ ] ")
  (evil-insert-state))

(defun mugu-orgi-insert-list-item ()
  "Focus on entry at point by expanding it while hiding other."
  (interactive)
  (newline-without-break-of-line)
  (insert "- ")
  (evil-insert-state))

(defun mugu-orgi-focus-headline ()
  "Focus on entry at point by expanding it while hiding other."
  (interactive)
  (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
    (save-excursion
      (org-overview))
    (org-cycle)
    (org-show-subtree)))

(defun mugu-orgi-counsel-agenda-files (&optional default-action)
  "Select an agenda file and apply it DEFAULT-ACTION.
DEFAULT-ACTION should be a mugu-orgu-action-afile-*.  If it isnt defined, no
action is performed."
  (let ((action (or default-action 'identity)))
    (ivy-read "Select an org agenda file : " (org-agenda-files)
              :action action
              :caller 'mugu)))

;; Menus
(defmenu mugu-orgi-menu-global (:color blue :hint nil)
  "Org mode external interface"
  ("cc" (org-roam-dailies-capture-today) "today" :column "Capture to journal")
  ("cd" (org-roam-dailies-capture-date) "a date")
  ("ct" (org-roam-dailies-capture-tomorrow) "tomorrow")
  ("cy" (org-roam-dailies-capture-yesterday) "yesterday")
  ("cl" org-store-link "a link to current location")
  ("CC" org-clock-cancel "cancel" :column "clocking")
  ("Co" org-clock-out "stop")
  ("fa" #'mugu-orgi-goto-agenda-file "an agenda file" :column "Find in side window")
  ("ff" (lambda () (interactive) (switch-to-buffer (mugu-orgu-get-last-buffer-name))) "last org file")
  ("ft" org-roam-dailies-goto-tomorrow "")
  ("fj" org-roam-dailies-goto-today "today note")
  ("fy" org-roam-dailies-goto-yesterday "yesterday note")
  ("fd" org-roam-dailies-goto-date "note at any date")
  ("fn" org-roam-node-find "a note")
  ("vv" mugu-feature-org-view-active-tasks "active taks" :column "tasks")
  ("vb" mugu-feature-org-view-backlog-tasks "backlog taks")
  ("vp" mugu-feature-org-view-planned-tasks "planned taks")
  ("vi" mugu-feature-org-view-icebox-tasks "icebox taks")
  ("n" mugu-feature-org-note "interface to org notes"))

(defmenu mugu-orgi-menu-agenda-major-mode (:color amaranth :hint nil)
  "Mugu"
  ("l" mugu-org-utils/agenda-forward-block "next block" :column "1-Navigation")
  ("h" mugu-org-utils/agenda-backward-block "previous block")
  ("j" org-agenda-next-item "next item")
  ("k" org-agenda-previous-item "previous item")
  ("J" org-agenda-drag-line-forward "drag up")
  ("K" org-agenda-drag-line-backward "drag bot")
  ("zz" org-agenda-recenter "recenter view")
  ("r" org-agenda-refile "refile" :column "2a-Task Actions")
  ("C-d" org-agenda-kill "kill")
  ("ds" org-schedule "set schedule date")
  ("dd" org-deadline "set deadline date")
  ("dt" org-time-stamp "set deadline date")
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
  ("=" org-agenda-filter-by-regexp "filter by regexp")
  ("<" org-agenda-filter-by-category "fitler by category")
  ("*" org-agenda-filter-by-top-headline "filter by top headline")
  ("g" org-agenda-goto "goto" :color blue :column "6-Terminating")
  ("RET" org-agenda-goto "goto" :color blue)
  ("q" org-agenda-quit "quit" :color blue)
  ("Q" org-agenda-Quit "quit" :color blue)
  ("x" org-agenda-exit "quit discard all change" :color blue)
  ("C-g" nil "exit this menu" :color blue))

(defmenu mugu-orgi-submenu-tree-actions
  (:color red :hint nil )
  "bindings for advanced tree manipulation"
  ("m" org-mark-element "✔ mark" :column "Marking")
  ("M" org-mark-subtree "✔✔ mark all")
  ("y" org-copy-subtree "copy subtree" :column "Copy")
  ("d" org-cut-subtree "cut subtree")
  ("p" org-paste-subtree "paste subtree")
  ("t" org-toggle-heading "morph headline/list" :column "Transform")
  ("s" mugu-orgw-sort-tasks "sort tasks")
  ("a" org-archive-subtree "archive subtree")
  ("/" org-sparse-tree "search" :color blue :column "Search")
  ("q" mugu-orgi-menu-org-major-mode "Return to org menu" :color blue :column nil))

(defmenu mugu-orgi-menu-org-major-mode
  (:color blue :hint nil)
  "bindings for ORG mode"
  ("e" (mugu-orgi-submenu-tree-actions) "(expert) tree manipulation" :column "Sub-menus")
  ("a" (mugu-orgi-submenu-headline-action) "headline actions" :color blue)
  ("ds" org-schedule "schedule" :column "Timing")
  ("dt" org-time-stamp "insert timestamp")
  ("dd" org-deadline "set deadline")
  ("r" org-refile "refile subtree" :column "Misc")
  ("t" org-todo "change TODO status")
  ("cc" org-ctrl-c-ctrl-c "ctrl-c²")
  ("cl" org-insert-link "insert link")
  ("mj" (mugu-orgw-move-headline-to-dailies (apply-partially #'org-roam-dailies-capture-today 'goto)) "checkout headline to today" :column "Dailies")
  ("mt" (mugu-orgw-move-headline-to-dailies (apply-partially #'org-roam-dailies-capture-tomorrow 1 'goto)) "checkout headline to tomorrow" )
  ("my" (mugu-orgw-move-headline-to-dailies (apply-partially #'org-roam-dailies-capture-yesterday 1 'goto)) "checkout headline to yesterday")
  ("md" (mugu-orgw-move-headline-to-dailies (apply-partially #'org-roam-dailies-capture-date 'goto)) "checkout headline to a date")
  ("j" org-roam-dailies-goto-next-note "next daily" :color red)
  ("k" org-roam-dailies-goto-previous-note "previous daily" :color red)
  ("f" (mugu-orgi-focus-headline) "focus")
  ("ii" org-insert-todo-heading "todo" :column "insert")
  ("is" org-insert-todo-subheading "sub-todo")
  ("ip" org-insert-heading "plain heading")
  ("ic" mugu-orgi-insert-checkbox "checkbox")
  ("iu" mugu-orgi-insert-list-item "item")
  ("*" (progn (org-insert-heading) (evil-insert-state)) "plain heading")
  ("-" (progn (org-insert-list-item) (evil-insert-state)) "item")
  ("il" mugu-orgi-action-insert-link-to-headline "link to headline")
  ("in" mugu-feature-org-insert-link-note "link to note")
  ("u" org-set-tags-command "update tags" :column "metadata")
  ("p" (org-priority) "set priority")
  ("P" org-set-property "set property")
  ("RET" org-insert-heading "insert" :column "Terminate")
  ("q" nil "exit"))

(defun mugu-orgi-set-configuration ()
  "Set org config value relative to interface."
  (setq org-use-fast-todo-selection t)
  (setq org-refile-use-outline-path t)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-indented t)
  (setq org-indirect-buffer-display 'current-window)
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-restore-windows-after-quit 't)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-align-tags-to-column -150)
  (setq org-agenda-tags-column -150)
  (setq org-tags-column -120)
  (setq org-agenda-log-mode-add-notes nil)
  (setq org-log-reschedule nil)
  (setq org-use-fast-todo-selection 'expert))

(defun mugu-orgi-configure-keys ()
  "Gather keys binding in one place."
  (general-def '(insert normal) org-mode-map
    "M-j" 'org-metadown
    "M-k" 'org-metaup
    "M-h" 'org-metaleft
    "M-l" 'org-metaright
    "M-g" 'org-promote-subtree
    "M-m" 'org-demote-subtree))

(provide 'mugu-org-interface)
;;; mugu-org-interface ends here
