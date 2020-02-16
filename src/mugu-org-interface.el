;; mugu-org-interface --- Front end to my org configuration  -*- lexical-binding: t -*-
;; tbc
;;; Commentary:
;;; Code:

(require 'mugu-buffer)
(require 'mugu-org-utils)
(require 'mugu-org-workflow)
(require 'mugu-menu)
(require 'ivy)
(require 'mugu-lisp-libs)
(require 'mugu-misc)
(require 'evil)
(require 'mugu-date-utils)

;; variables
(defvar mugu-orgi-last-command nil
  "Last org interface command invoked.")
(defvar mugu-orgi-current-headline nil
  "Current selected headline for debug purposes.")
(defvar mugu-orgi-is-local nil
  "Indicates if opeartions and searchs consider all agenda files or current one.")

(defvar mugu-orgi-headline-actions
  `(("a" mugu-orgw-set-active "set Active" 'persistant "Basic actions")
    ("p" mugu-orgu-set-priority "set Priority" 'persistant)
    ("t" mugu-orgu-change-todo-state "set Todo" 'persistant)
    ("ds" mugu-orgi-schedule "schedule" 'persistant "Scheduling")
    ("dd" mugu-orgi-deadline "deadline" 'persistant)
    ("dr" mugu-orgw-delete-timestamp "reset task" 'persistant)
    ("ri" ,(apply-partially #'mugu-orgi--action-refile-headline #'mugu-orgw-inbox-headline-p) "Refile to Inbox" 'persistant "Refiling")
    ("rt" ,(apply-partially #'mugu-orgi--action-refile-headline #'mugu-orgw-is-planified-p) "Refile to Task" 'persistant)
    ("ft" mugu-orgw-select-for-transport "for transport" 'persistant)
    ("mb" mugu-orgi-move-to-backlog "Move to backlog" 'persistant "Moving")
    ("mi" mugu-orgi-move-to-icebox "Move to icebox" 'persistant)
    ("ma" mugu-orgi-move-to-archive "Move to archive" 'persistant)
    ("c" mugu-orgw-capture-to-headline "capture to headline" "Capturing to"))
  "A list of possible actions for a given headline.
Each action has the form: hotkey function description persistance column.
Absence of persistance indicates the action interrupts the ivy session upon
resolution.
Column indicates where the action should be located on the menu.")

;; hacks
(defun mugu-orgi-switch-to-buffer-other-window (_orig-fun &rest args)
  "Ugly but the original implementation popped frame when side window was open.
This is because of the macro `org-no-popups' which actually did the opposite of
what it was intended to do.  Since it also unbound `display-buffer-alist' it was
not possible to fix it otherwise.
ORIG-FUN and ARGS are not read."
  (apply #'switch-to-buffer-other-window args))

;; utilitites
(defun mugu-orgi--prepare-headlines-for-ivy (headlines)
  "Format HEADLINES to ivy expected format of a list of (candidate . object).
Also sort the collection by urgency."
  (let ((total-headlines (length headlines)))
    (--map-indexed (cons (mugu-orgi--display-headline it it-index total-headlines) it)
                   (-sort 'mugu-orgw-cmp-headlines headlines))))

(defun mugu-orgi--display-headline (headline index total)
  "Return a string describing HEADLINE.
The current INDEX vs TOTAL of the headline will also be displayed."
  (let* ((outline (mapconcat (lambda (h) (org-element-property :raw-value h))
                             (reverse (org-element-lineage headline nil 'with-self))
                             " > "))
         (filename (s-capitalize (file-name-base (mugu-orgu-get-file headline))))
         (outline-with-file (s-concat filename " > " (substring outline 3)))
         (todo-segment (format "[ %s ]" (or (org-element-property :todo-keyword headline)
                                            "NONE")))
         (outline-segment (truncate-string-to-width outline-with-file 150 0 ?\ ))
         (tags-segment (truncate-string-to-width
                        (s-join "/" (--map (format "@%s" it) (mugu-orgu-get-tags headline 'inherit)))
                        20 0 ?\ ))
         (index-segment (format "(%s/%s)" index total))
         (pretty-outline (format "%s %s %s %s"
                                 todo-segment
                                 outline-segment
                                 tags-segment
                                 index-segment)))
    pretty-outline))

(defun mugu-orgi--counsel-headlines (headlines default-action)
  "Select a headline from HEADLINES and apply it preselected DEFAULT-ACTION.
DEFAULT-ACTION should accept an org-element headline as single argument.
Headlines are displayed sorted according to their urgentness."
  (let* ((action (or default-action 'identity))
         (ivy-prescient-sort-commands))
    (ivy-read "select an headline: "
              (mugu-orgi--prepare-headlines-for-ivy headlines)
              :action (lambda (candidate) (funcall action (cdr candidate)))
              :initial-input ""
              :caller 'mugu-orgi)))

(defun mugu-orgi-counsel--pick-headlines (headlines)
  "Select a headline from HEADLINES and return its `org-element' object."
  (let* ((headline-object nil)
         (action (lambda (candidate) (setq headline-object candidate))))
    (mugu-orgi--counsel-headlines headlines action)
    headline-object))

(defmacro mugu-orgi--make-command (name headlines-query default-action)
  "Make a org ivy command named NAME.
It applies only to headlines returned by HEADLINES-QUERY with DEFAULT-ACTION
upon selection."
  `(defun ,name ()
     ,(format "Automatically generated function to select headline.
headline predicate : %s
headline action : %s." headlines-query default-action)
     (interactive)
     (setq mugu-orgi-last-command ',name)
     (mugu-orgi--counsel-headlines (funcall ,headlines-query) ,default-action)))

(defun mugu-orgi--to-ivy-action (headline-action &optional persistant)
  "Wrap a HEADLINE-ACTION so it can be invoked as a ivy action.
HEADLINE-ACTION is a function taking a single headline as argument and should
return the updated headline.
The last command session is resumed after if PERSISTANT is not nil."
  (lambda (headline)
    (funcall headline-action (cdr headline))
    (when persistant (funcall mugu-orgi-last-command))))

(defun mugu-orgi--select-tags (tag-or-predicate)
  "Return TAG-OR-PREDICATE interactivly if needed."
  (if (functionp tag-or-predicate)
      (ivy-read "Select a tag"
                (mugu-orgw-global-tags-list tag-or-predicate))
    tag-or-predicate))

(defun mugu-orgi--make-query (headlines-predicate)
  "Make a query returning headlines matching HEADLINES-PREDICATE."
  (lambda () (mugu-orgw-list-headlines headlines-predicate mugu-orgi-is-local)))

(defun mugu-orgi--make-query-by-tags (tag-or-predicate)
  "Make a query returning headlines matching TAG-OR-PREDICATE."
  (lambda () (mugu-orgw-list-headlines
              (apply-partially #'mugu-orgw-with-tag-p
                               (mugu-orgi--select-tags tag-or-predicate))
              mugu-orgi-is-local)))

;;; Actions
(defun mugu-orgi--action-focus-headline (headline)
  "Go to HEADLINE and focus on it."
  (interactive)
  (mugu-orgu-action-headline-goto headline)
  (mugu-orgi-focus-headline))

(defsubst mugu-orgi-schedule (headline)
  "Schedule the HEADLINE or the task at point if called interractively."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgu-schedule headline (mugu-date-utils-read-date)))

(defsubst mugu-orgi-deadline (headline)
  "Define deadline for the HEADLINE or the task at point if called interractively."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgu-deadline headline (mugu-date-utils-read-date)))

(defun mugu-orgi-move-to-icebox (headline)
  "Move HEADLINE to its associated inbox."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgw-move-to-box #'mugu-orgw-icebox-p headline))

(defun mugu-orgi-move-to-archive (headline)
  "Move HEADLINE to its associated archive."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgw-move-to-box #'mugu-orgw-archive-p headline))

(defun mugu-orgi-move-to-backlog (headline)
  "Move HEADLINE to its associated backlog."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgw-move-to-box #'mugu-orgw-backlog-p headline))

(defun mugu-orgi--action-refile-headline (target-headline-p headline)
  "Refile HEADLINE to another headline matching predicate TARGET-HEADLINE-P."
  (mugu-orgu-action-headline-refile headline
                                    (mugu-orgi-counsel--pick-headlines (mugu-orgw-list-headlines target-headline-p))))

(defun mugu-orgi--action-copy-headline (target-headline-p headline)
  "Copy HEADLINE to another headline matching predicate TARGET-HEADLINE-P."
  (mugu-orgu-action-headline-copy headline
                                    (mugu-orgi-counsel--pick-headlines (mugu-orgw-list-headlines target-headline-p))))

(defmacro mugu-orgi--make-headline-list (name predicate)
  "Make a function with NAME returning a list of headlines matching PREDICATE.
If `mugu-orgi-is-local' is non-nil, restrict search to current file."
  `(defun ,name ()
     ,(format "Return all headlines matching %s." predicate)
     (mugu-orgw-list-headlines ,predicate mugu-orgi-is-local)))


;; Headlines list selectors
(mugu-orgi--make-headline-list mugu-orgi-inbox-headlines #'mugu-orgw-inbox-p)
(mugu-orgi--make-headline-list mugu-orgi-icebox-headlines #'mugu-orgw-icebox-p)
(mugu-orgi--make-headline-list mugu-orgi-backlog-headlines #'mugu-orgw-backlog-p)
(mugu-orgi--make-headline-list mugu-orgi-archive-headlines #'mugu-orgw-archive-p)
(mugu-orgi--make-headline-list mugu-orgi-in-inbox-tasks (apply-partially #'mugu-orgu-all-p
                                                                         #'mugu-orgw-in-inbox-p
                                                                         #'mugu-orgw-task-p))
(mugu-orgi--make-headline-list mugu-orgi-in-icebox-tasks (apply-partially #'mugu-orgu-all-p
                                                                          #'mugu-orgw-in-icebox-p
                                                                          #'mugu-orgw-task-p))
(mugu-orgi--make-headline-list mugu-orgi-in-backlog-tasks (apply-partially #'mugu-orgu-all-p
                                                                           #'mugu-orgw-in-backlog-p
                                                                           #'mugu-orgw-task-p))
(mugu-orgi--make-headline-list mugu-orgi-in-archive-tasks (apply-partially #'mugu-orgu-all-p
                                                                           #'mugu-orgw-in-archive-p
                                                                           #'mugu-orgw-task-p))
(mugu-orgi--make-headline-list mugu-orgi-schedulable-tasks #'mugu-orgw-schedulable-p)
(mugu-orgi--make-headline-list mugu-orgi-planified-tasks #'mugu-orgw-is-planified-p)
(mugu-orgi--make-headline-list mugu-orgi-wait-tasks (apply-partially #'mugu-orgu-all-p #'mugu-orgw-task-p #'mugu-orgw-wait-p))
(mugu-orgi--make-headline-list mugu-orgi-any-tasks #'mugu-orgu-todo-headline-p)
(mugu-orgi--make-headline-list mugu-orgi-any-headlines #'identity)
(mugu-orgi--make-headline-list mugu-orgi-done-yesterday-headlines (apply-partially #'mugu-orgu-all-p
                                                                                   #'mugu-orgw-done-yesterday-p
                                                                                   #'mugu-orgw-task-p))

(defun mugu-orgi-current-task-subtasks ()
  "List all subchilds task of current task."
  (mugu-orgu-list-childs (mugu-orgw-current-task) #'mugu-orgu-todo-headline-p 'with-parent))

(defun mugu-orgi-current-task-subheadings ()
  "List all subchild headings of current task."
  (mugu-orgu-list-childs (mugu-orgw-current-task) #'identity 'with-parent))

;;; Commands
(mugu-orgi--make-command mugu-orgi-goto-inbox-headlines #'mugu-orgi-inbox-headlines #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-icebox-headlines #'mugu-orgi-icebox-headlines #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-backlog-headlines #'mugu-orgi-backlog-headlines #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-archive-headlines #'mugu-orgi-archive-headlines #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-in-inbox-tasks #'mugu-orgi-in-inbox-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-in-icebox-tasks #'mugu-orgi-in-icebox-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-in-backlog-tasks #'mugu-orgi-in-backlog-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-in-archive-tasks #'mugu-orgi-in-archive-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-schedulable-tasks #'mugu-orgi-schedulable-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-planified-tasks #'mugu-orgi-planified-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-wait-tasks #'mugu-orgi-wait-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-any-tasks #'mugu-orgi-any-tasks #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-any-headlines #'mugu-orgi-any-headlines #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-done-yesterday #'mugu-orgi-done-yesterday-headlines #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-current-task-subtasks
                         #'mugu-orgi-current-task-subtasks
                         #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-current-task-subheadlines
                         #'mugu-orgi-current-task-subheadings
                         #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-standup-headline
                         (mugu-orgi--make-query-by-tags "@standup")
                         #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-transport-headline
                         (mugu-orgi--make-query-by-tags "@transport")
                         #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-headline-by-tag
                         (mugu-orgi--make-query-by-tags 'identity)
                         #'mugu-orgi--action-focus-headline)
(mugu-orgi--make-command mugu-orgi-goto-headline-by-context-tag
                         (mugu-orgi--make-query-by-tags (apply-partially #'s-starts-with? "@"))
                         #'mugu-orgi--action-focus-headline)

(defsubst mugu-orgi-goto-current-task ()
  "Goto the current task."
  (interactive)
  (mugu-orgu-action-headline-goto (mugu-orgw-current-task)))

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
    (org-cycle)))

(defun mugu-orgi-counsel-agenda-files (&optional default-action)
  "Select an agenda file and apply it DEFAULT-ACTION.
DEFAULT-ACTION should be a mugu-orgu-action-afile-*.  If it isnt defined, no
action is performed."
  (let ((action (or default-action 'identity)))
    (ivy-read "Select an org agenda file : " (org-agenda-files)
              :action action)))

;; Menus
(defmenu mugu-orgi-menu-goto-headlines (:color blue :hint nil)
  "Virtual hydra to select and go to an headline"
  ("gg" (mugu-orgi-goto-schedulable-tasks) "schedulable" :column "Goto important tasks")
  ("gp" (mugu-orgi-goto-planified-tasks) "planified")
  ("gw" (mugu-orgi-goto-wait-tasks) "waiting")
  ("gr" (mugu-orgi-goto-in-inbox-tasks) "refilable")
  ("ga" (mugu-orgi-goto-current-task) "goto current task" :column "Goto current task")
  ("gss" (mugu-orgi-goto-current-task-subheadlines) "goto sub headlines")
  ("gst" (mugu-orgi-goto-current-task-subtasks) "goto subtasks")
  ("ss" (mugu-orgi-goto-inbox-headlines) "inbox" :column "Goto a box")
  ("sb" (mugu-orgi-goto-backlog-headlines) "backlog" )
  ("si" (mugu-orgi-goto-icebox-headlines) "icebox" )
  ("sa" (mugu-orgi-goto-archive-headlines) "archive")
  ("ll" (mugu-orgi-goto-in-inbox-tasks) "inbox" :column "Goto tasks in box")
  ("lb" (mugu-orgi-goto-in-backlog-tasks) "backlog")
  ("li" (mugu-orgi-goto-in-icebox-tasks) "icebox" )
  ("la" (mugu-orgi-goto-in-archive-tasks) "archive" )
  ("at" (mugu-orgi-goto-any-tasks) "task" :column "Goto any")
  ("ah" (mugu-orgi-goto-any-headlines) "goto any task")
  ("ts" (mugu-orgi-goto-headline-by-tag) "goto headline by tag" :column "Goto by tag")
  ("tc" (mugu-orgi-goto-headline-by-context-tag) "goto headline by context tag")
  ("tt" (mugu-orgi-goto-transport-headline) "goto headline for transport")
  ("tm" (mugu-orgi-goto-standup-headline) "goto headline for morning standup"))

(defmenu mugu-orgi-menu-global (:color blue :hint nil
                                       :inherit (mugu-orgi-menu-goto-headlines-hydra/heads)
                                       :body-pre (setq mugu-orgi-is-local nil))
  "Org mode external interface"
  ("Aa" (mugu-orgw-agenda-future-overview) "global agenda" :column "Agenda")
  ("At" (mugu-orgw-agenda-today-overview) "global agenda" :column "Agenda")
  ("cc" (mugu-orgw-capture-todo #'mugu-orgi-goto-inbox-headlines) "to inbox" :column "Capture")
  ("cb" (mugu-orgw-capture-todo #'mugu-orgi-goto-backlog-headlines) "to backlog")
  ("ci" (mugu-orgw-capture-todo #'mugu-orgi-goto-icebox-headlines) "to icebox")
  ("cs" (mugu-orgw-capture-todo #'mugu-orgi-goto-current-task-subtasks) "to current task")
  ("ca" (mugu-orgw-capture-todo #'mugu-orgi-goto-schedulable-tasks) "to active task")
  ("fa" (mugu-orgi-goto-agenda-file) "goto agenda files" :column "Goto File")
  ("ff" (mugu-buffer-switch (mugu-orgu-get-last-buffer-name)) "goto last visited"))

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
  ("s" (progn (mugu-orgu-sort-subtree 'mugu-orgw-cmp-headlines)
              (mugu-orgi-menu-org-major-mode)) "entry sort" :color blue)
  ("S" (progn (mugu-orgu-sort-subtree 'mugu-orgw-cmp-headlines 'recursive)
              (mugu-orgi-menu-org-major-mode)) "recursive subtree sort" :color blue)
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
  ("c" org-ctrl-c-ctrl-c "ctrl-c²")
  ("f" (mugu-orgi-focus-headline) "focus")
  ("ii" org-insert-todo-heading "todo" :column "insert")
  ("is" org-insert-todo-subheading "sub-todo")
  ("ip" org-insert-heading "plain heading")
  ("ic" mugu-orgi-insert-checkbox "checkbox")
  ("il" mugu-orgi-insert-list-item "item")
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
    (:color blue :hint nil :after-exit (mugu-orgi-menu-org-major-mode))
   ,@(--map (list (-first-item it)
                  (lambda () (interactive) (funcall (-second-item it) (mugu-orgu-element-at-point)))
                  (-third-item it)
                  (when (-fifth-item it) :column)
                  (when (-fifth-item it) (-fifth-item it)))
            mugu-orgi-headline-actions)))

(defun mugu-orgi-toggle-private ()
  "Activate or deactivate private task."
  (interactive)
  (if (eq mugu-orgw-forbidden-headline-p-function #'mugu-orgw-private-headline-p)
      (setq mugu-orgw-forbidden-headline-p-function #'ignore)
    (setq mugu-orgw-forbidden-headline-p-function #'mugu-orgw-private-headline-p)))

(defun mugu-orgi-toggle-work ()
  "Activate or deactivate private task."
  (interactive)
  (if (eq mugu-orgw-forbidden-headline-p-function #'mugu-orgw-work-headline-p)
      (setq mugu-orgw-forbidden-headline-p-function #'ignore)
    (setq mugu-orgw-forbidden-headline-p-function #'mugu-orgw-work-headline-p)))

(defun mugu-orgi--configure-ivy ()
  "Customize Ivy experience."
  (ivy-set-actions 'mugu-orgi
                   (--map (list (-first-item it)
                                (mugu-orgi--to-ivy-action (-second-item it) (-fourth-item it))
                                (-third-item it))
                          mugu-orgi-headline-actions)))

(defun mugu-orgi-select-headline-at-point ()
  "Store headline at point  in `mugu-orgi-current-headline'."
  (interactive)
  (setq mugu-orgi-current-headline (mugu-orgu-element-at-point)))

(defun mugu-orgi-set-configuration ()
  "Set org config value relative to interface."
  (setq org-use-fast-todo-selection t)
  (setq org-refile-use-outline-path t)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
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
  (setq org-use-fast-todo-selection 'expert)

  (mugu-window-configure-side-window "CAPTURE*" 'bottom 0.2)
  (mugu-window-configure-side-window "\\*Org todo\\*" 'bottom 0.1)
  (mugu-window-configure-side-window ".*.org$" 'bottom 0.6 t)

  (advice-add #'org-switch-to-buffer-other-window :around #'mugu-orgi-switch-to-buffer-other-window)
  (mugu-orgi--configure-ivy))

(defun mugu-orgi-configure-keys ()
  "Gather keys binding in one place."
  ;; (setq org-agenda-ignore-drawer-properties '(effort appt category))
  (mugu-date-utils-configure)
  (general-def org-mode-map
    "M-j" 'org-move-subtree-down
    "M-k" 'org-move-subtree-up
    "M-h" 'org-do-promote
    "M-l" 'org-do-demote
    "M-g" 'org-promote-subtree
    "M-m" 'org-demote-subtree))

(defun mugu-orgi-test-predicate (headline)
  ".
HEADLINE."
  (interactive (list (mugu-orgu-element-at-point)))
  (if (mugu-orgw-task-p headline)
      (message "It matches")
    (message "It does not matches")))

(provide 'mugu-org-interface)
;;; mugu-org-interface ends here
