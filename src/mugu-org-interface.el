;; mugu-org-interface --- Front end to my org configuration  -*- lexical-binding: t -*-
;; tbc
;;; Commentary:
;;; Code:

(require 'mugu-org-workflow)
(require 'mugu-menu)
(require 'mugu-feature)
(require 'mugu-org-sql)
(require 'ivy)
(require 'mugu-misc)
(require 'evil)
(require 'mugu-date-utils)
(require 'f)

;; variables
(defvar mugu-orgi-last-view nil
  "Last org sql view visited.")
(defvar mugu-orgi-is-local nil
  "Indicates if opeartions and searchs consider all agenda files or current one.")
(defvar mugu-orgi-cached-multi-action-args nil
  "Stores arguments for mutli actions.")

(define-mugu-feature org-note)
(define-mugu-feature org-insert-note-link)

(defvar mugu-orgi-headline-actions
  `(("a" org-clock-in "set Active" 'persistant "Basic actions")
    ("p" mugu-orgu-priority "set Priority")
    ("t" org-todo "set Todo")
    ("n" (lambda (&optional _) (interactive) (mugu-orgi--action-note-wrapper)) "add note")
    ("A" org-archive-subtree "archive subtree" 'persistant "Destructive actions")
    ("D" org-archive-subtree "delete subtree" 'persistant)
    ("ci" org-clock-in "Clock in" 'persistant "Clocking")
    ("co" org-clock-out "Clock out" 'persistant)
    ("ds" org-schedule "schedule" 'persistant "Scheduling")
    ("dd" org-deadline "deadline" 'persistant)
    ("rr" mugu-orgi-action-refile  "Refile to Headline" nil "Refiling")
    ("rf" mugu-orgi-action-refile-to-file "Refile to File" nil))
  "A list of possible actions for a given headline.
Each action has the form: hotkey function description persistance column.
Absence of persistance indicates the action interrupts the ivy session upon
resolution.
Column indicates where the action should be located on the menu.")

(defvar mugu-orgi-headline-ivy-actions
  '(("p" mugu-orgi-action-set-priority "Change priority")
    ("rr" mugu-orgi-action-refile "Refile to another headline")
    ("rf" mugu-orgi-action-refile-to-file "Refile to another file")))

;;; Actions
(defun mugu-orgi--action-note-wrapper ()
  "Wrapper for compatibility to take note from anywhere.
Kinda hackish because add note is implemented with hooks?"
  (org-add-note)
  (remove-hook 'post-command-hook 'org-add-log-note)
  (org-add-log-note))

(defun mugu-orgi--action-focus-headline (headline)
  "Go to HEADLINE and focus on it."
  (interactive)
  (mugu-orgu-action-headline-goto headline)
  (mugu-orgi-focus-headline))

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
(defmenu mugu-orgi-menu-goto-headlines (:color blue :hint nil)
  "Virtual hydra to select and go to an headline"
  ("gc" org-clock-goto "current" :column "Goto current tasks")
  ("ga" (mugu-orgi-select-headlines 'active_tasks) "active")
  ("gg" (mugu-orgi-select-headlines 'todo_tasks) "all" )
  ("gw" (mugu-orgi-select-headlines 'reminder_tasks) "waiting" :column "Goto upcoming tasks")
  ("gi" (mugu-orgi-select-headlines 'inbox_tasks) "inbox")
  ("gp" (mugu-orgi-select-headlines 'planned_tasks) "planned")
  ("gb" (mugu-orgi-select-headlines 'backlog_tasks) "backlog")
  ("gr" (mugu-orgi-select-headlines 'upcoming_reminder_tasks) "upcoming")
  ("at" (mugu-orgi-select-headlines 'all_tasks) "task" :column "Goto any")
  ("ah" (mugu-orgi-select-headlines 'all_headlines) "headline"))

(defmenu mugu-orgi-menu-global (:color blue :hint nil
                                       :inherit (mugu-orgi-menu-goto-headlines-hydra/heads)
                                       :body-pre (setq mugu-orgi-is-local nil))
  "Org mode external interface"
  ("cc" (org-roam-dailies-capture-today) "today" :column "Capture to journal")
  ("cd" (org-roam-dailies-capture-date) "a date")
  ("ct" (org-roam-dailies-capture-tomorrow) "tomorrow")
  ("cy" (org-roam-dailies-capture-yesterday) "yesterday")
  ("cn" (mugu-roam-capture-daily-note) "a note" :column "Capture (misc)")
  ("cl" org-store-link "a link to current location")
  ("CC" org-clock-cancel "cancel" :column "clocking")
  ("Co" org-clock-out "stop")
  ("fa" #'mugu-orgi-goto-agenda-file "an agenda file" :column "Find in side window")
  ("ff" (lambda () (interactive) (switch-to-buffer (mugu-orgu-get-last-buffer-name))) "last org file")
  ("ft" (mugu-orgi-select-headlines-by-tag "quicktip") "quick tip")
  ("fj" org-roam-dailies-find-today "today note")
  ("fy" org-roam-dailies-find-yesterday "yesterday note")
  ("fd" org-roam-dailies-date "note at any date")
  ("fn" org-roam-find-file "a note")
  ("fi" (mugu-orgi-select-headlines-by-tag "quickfix") "quick fix")
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

(defmenu  mugu-orgi-menu-hjkl (:color pink :hint nil)
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

(defmenu mugu-orgi-submenu-tree-actions
  (:color red :hint nil :inherit (mugu-orgi-menu-hjkl-hydra/heads))
  "bindings for advanced tree manipulation"
  ("m" org-mark-element "✔ mark" :column "Marking")
  ("M" org-mark-subtree "✔✔ mark all")
  ("y" org-copy-subtree "copy subtree" :column "Copy")
  ("d" org-cut-subtree "cut subtree")
  ("p" org-paste-subtree "paste subtree")
  ("t" org-toggle-heading "morph headline/list" :column "Transform")
  ("s" mugu-orgw-sort-tasks "sort tasks")
  ;; ("s" (progn (mugu-orgu-sort-subtree 'mugu-orgw-cmp-headlines)
  ;;             (mugu-orgi-menu-org-major-mode)) "entry sort" :color blue)
  ;; ("S" (progn (mugu-orgu-sort-subtree 'mugu-orgw-cmp-headlines 'recursive)
  ;;             (mugu-orgi-menu-org-major-mode)) "recursive subtree sort" :color blue)
  ("a" org-archive-subtree "archive subtree")
  ("/" org-sparse-tree "search" :color blue :column "Search")
  ("q" mugu-orgi-menu-org-major-mode "Return to org menu" :color blue :column nil))

(defmenu mugu-orgi-menu-org-major-mode
  (:color blue :hint nil :inherit (mugu-orgi-menu-hjkl-hydra/heads))
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
  ;; ("a" org-attach "attach interface" :column "Insert")
  ;; ("*" org-insert-heading "insert heading")
  ;; ("l" org-insert-link "insert link")
  ;; ("n" org-add-note "insert note")
  ("u" org-set-tags-command "update tags" :column "metadata")
  ("p" (org-priority) "set priority")
  ("P" org-set-property "set property")
  ("RET" org-insert-heading "insert" :column "Terminate")
  ("o" (mugu-orgi-org-file-menu) "file wide operation")
  ("q" nil "exit"))

(defmenu mugu-orgi-org-file-menu
  (:color blue :hint nil :inherit (mugu-orgi-menu-goto-headlines-hydra/heads)
          :body-pre (setq mugu-orgi-is-local t))
  "Org mode file wide operation")

(eval
 `(defmenu mugu-orgi-submenu-headline-action
    (:color blue :hint nil)
   ,@(--map (list (-first-item it)
                  (lambda () (interactive)
                    (funcall (-second-item it) (mugu-orgu-element-at-point))
                    (when (-fourth-item it) (mugu-orgi-menu-org-major-mode))
                    )
                  (-third-item it)
                  (when (-fifth-item it) :column)
                  (when (-fifth-item it) (-fifth-item it)))
            mugu-orgi-headline-actions)))

(defun mugu-orgi-set-configuration ()
  "Set org config value relative to interface."
  (setq org-use-fast-todo-selection t)
  (setq org-refile-use-outline-path t)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-indented t)
  ;;Use the current window for indirect buffer display
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
  ;; (setq org-agenda-ignore-drawer-properties '(effort appt category))
  (mugu-date-utils-configure)
  (general-def '(insert normal) org-mode-map
    "M-j" 'org-metadown
    "M-k" 'org-metaup
    "M-h" 'org-metaleft
    "M-l" 'org-metaright
    "M-g" 'org-promote-subtree
    "M-m" 'org-demote-subtree))

;; ivy integration
(defun mugu-orgi--action-goto (candidate)
  "Goto headline CANDIDATE and focus it."
  (funcall (mugu-orgi--convert-to-ivy-action #'mugu-orgu-goto-headline) candidate)
  (mugu-orgi-focus-headline))

(defun mugu-orgi-action-insert-link-to-headline ()
  "Insert a link to target headline."
  (interactive)
  (--> (mugu-orgi-pick-headline)
       (insert (format "[[file:%s::%s][%s]]"
                             (plist-get it :file_path)
                             (plist-get it :headline_text)
                             (plist-get it :headline_text)))))

(defun mugu-orgi-pick-headline (&optional view-name)
  "Select an headline from VIEW-NAME."
  (let* ((headline nil)
         (action (lambda (candidate) (setq headline (cdr candidate)))))
    (mugu-orgi-select-headlines (or view-name 'all_headlines) :default-action action)
    headline))

(defmacro with-cached-multi-action-args (&rest body)
  "Eval BODY and return it's results or use its cached version."
  `(setq mugu-orgi-cached-multi-action-args
         (or mugu-orgi-cached-multi-action-args
             ,@body)))

;; Headlines actions
(defun mugu-orgi-action-refile (headline)
  "Refile HEADLINES interactively."
  (interactive)
  (mugu-orgu-refile
   (with-cached-multi-action-args
    (let ((ivy-last))
      (mugu-orgi-pick-headline 'all_headlines)))
   headline))

(defun mugu-orgi-action-refile-to-file (headline)
  "Refile HEADLINE to a file interactively."
  (mugu-orgu-refile-to-file
   (let ((ivy-last))
     (with-cached-multi-action-args (mugu-orgi-counsel-agenda-files)))
   headline))

(defun mugu-orgi-read-priority ()
  (progn (message "Priority %c-%c, SPC to remove: "
                  org-priority-highest org-priority-lowest)
         (save-match-data
           (setq new (read-char-exclusive)))))

(defun mugu-orgi-action-set-priority (headline)
  "Define priority for HEADLINE."
  (interactive)
  (mugu-orgu-priority
   (with-cached-multi-action-args (mugu-orgi-read-priority))
   headline))

(defun mugu-orgi-refile-headline ()
  (interactive)
  (mugu-orgi-select-headlines 'all_headlines :default-action #'mugu-orgi-action-refile))

(defun mugu-orgi--headlines-for-ivy (view-name predicate)
  "Return a alist of headlines formatted forv ivy corresponding to VIEW-NAME."
  (--> (mugu-org-sql-select-from 'all_headlines)
       (if mugu-orgi-is-local
           (-filter (lambda (p-hl) (f-equal? buffer-file-name (plist-get p-hl :file_path))) it)
         it)
       (-filter predicate it)
       (-map (lambda (p-hl) (cons (mugu-org-sql-headline-repr p-hl) p-hl)) it)))

(cl-defun mugu-orgi-select-headlines (view-name &key default-action headline-predicate)
  "Interactively select an org headline with ivy corresponding to VIEW-NAME.
Perform DEFAULT-ACTION on the selected headline.
If HEADLINE-PREDICATE is non-nil headlines will be filtered according to it."
  (interactive (list 'all_tasks))
  (setq mugu-orgi-last-view view-name)
  (let ((mugu-orgi-cached-multi-action-args))
    (--> (mugu-orgi--headlines-for-ivy view-name (or headline-predicate #'identity))
         (ivy-read "Headlines: " it
                   :action (or default-action #'mugu-orgi--action-goto)
                   :caller 'mugu-orgi-headlines))))

(defun mugu-orgi-select-headlines-by-tag (tag)
  "Select all headlines matching TAG."
  (mugu-orgi-select-headlines 'all_headlines :headline-predicate (mugu-orgu-make-tag-predicate tag)))

(defun mugu-orgi--convert-to-ivy-action (standard-action)
  "Transform STANDARD-ACTION to an action that can be used by ivy.
KEEP-SESSION non nil indicates to keep the current ivy open."
  (lambda (candidate-or-cons)
    (--> candidate-or-cons
         (or (and (consp it) (cdr it)) it)
         (funcall standard-action it))))

(defun mugu-orgi-define-ivy-actions ()
  "Define all ivy actions."
  (->> mugu-orgi-headline-ivy-actions
       (-map (-lambda ((key func desc _))
               (list key
                     (mugu-orgi--convert-to-ivy-action func)
                     desc)))
       (ivy-set-actions 'mugu-orgi-headlines)))

(defun mugu-ivy-org-candidates (candidate)
  (or
   (ivy-more-chars)
   (--> (mugu-org-sql-cmd-select 'all_headlines mugu-org-sql-base-flat-cols (list (sql-matcher-fts-standard candidate)))
        (-map (lambda (p-hl) (cons (mugu-org-sql-headline-repr p-hl) p-hl)) it))))

(provide 'mugu-org-interface)
;;; mugu-org-interface ends here
