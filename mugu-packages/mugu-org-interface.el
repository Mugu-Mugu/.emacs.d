;; mugu-org-interface --- Summary
;; tbc
;;; Commentary:
;;; Code:

(require 'mugu-org-utils)
(require 'mugu-org-workflow)
(require 'mugu-menu)
(require 'ivy)
(require 'mugu-lisp-libs)

(defun mugu-orgi-switch-to-buffer-other-window (orig-fun &rest args)
  "Ugly but the original implementation popped frame when side window was open.
This is because of the macro `org-no-popups' which actually did the opposite of
what it was intended to do.  Since it also unbound `display-buffer-alist' it was
not possible to fix it otherwise.
ORIG-FUN and ARGS are not read."
  (apply #'switch-to-buffer-other-window args))

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
  (cl-letf (((symbol-function 'message) (lambda (&rest args) nil)))
    (save-excursion
      (org-overview))
    (org-cycle)))

(defun mugu-orgi-goto-focus-headline (headline)
  "Go to HEADLINE and focus on it."
  (interactive)
  (mugu-orgu-action-headline-goto headline)
  (mugu-orgi-focus-headline))

(defun mugu-orgi--annotate-headline-for-ivy (headline)
  "Return a cons cell where car is an outline of HEADLINE and cdr is the HEADLINE."
  (cons (mugu-orgu-get-outline headline) headline))

(defun mugu-orgi-counsel-headlines (headlines default-action)
  "Select a headline from HEADLINES and apply it preselected DEFAULT-ACTION.
DEFAULT-ACTION should accept an org-element headline as single argument.
Headlines are displayed sorted according to their urgentness."
  (let* ((annoted-headlines (--map (mugu-orgi--annotate-headline-for-ivy it)
                                   (-sort 'mugu-orgw-sort-cmp-headlines headlines)))
         (action (or default-action 'identity)))
    (ivy-read "select an headline: "
              annoted-headlines
              :action (lambda (candidate) (funcall action (cdr candidate)))
              :initial-input "")))

(defun mugu-orgi-counsel-agenda-files (&optional default-action)
  "Select an agenda file and apply it DEFAULT-ACTION.
DEFAULT-ACTION should be a mugu-orgu-action-afile-*.  If it isnt defined, no
action is performed."
  (let ((action (or default-action 'identity)))
    (ivy-read "Select an org agenda file : " (org-agenda-files)
              :action action)))

(defmacro mugu-orgi-make-headline-query (name headline-p headline-action)
  "Build a new command NAME that interactivly select headline and act on it.
HEADLINE-P should be a function that determine if a candidate headline should be
selected.  It should accept a single argument: an org-element headline.
HEADLINE-ACTION should be a function that act upon an org-element item."
  `(progn
     (defun ,(intern (format "%s-local" name)) ()
       ,(format "Automatically generated function to select headline.
headline predicate : %s
headline action : %s" headline-p headline-action)
       (interactive)
       (mugu-orgi-counsel-headlines (mugu-orgu-list-headlines-local ,headline-p)
                                    ,headline-action))
     (defun ,name ()
       ,(format "Automatically generated function to select headline.
headline predicate : %s
headline action : %s" headline-p headline-action)
       (interactive)
       (mugu-orgi-counsel-headlines (mugu-orgu-list-headlines ,headline-p)
                                    ,headline-action))))
(mugu-orgi-make-headline-query mugu-orgi-goto-leaf-headline
                               #'mugu-orgw-leaf-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-refile-refilable-task
                               #'mugu-orgw-refilable-headline-p #'mugu-orgu-action-headline-refile)
(mugu-orgi-make-headline-query mugu-orgi-goto-refilable-task
                               #'mugu-orgw-refilable-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-active-task
                               #'mugu-orgw-active-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-project-task
                               #'mugu-orgw-project-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-top-project-task
                               #'mugu-orgw-top-project-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-leaf-project-task
                               #'mugu-orgw-leaf-project-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-stuck-project-task
                               #'mugu-orgw-stuck-project-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-next-task
                               #'mugu-orgw-next-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-wait-task
                               #'mugu-orgw-wait-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-todo-task
                               #'mugu-orgw-todo-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-any-todo
                               #'mugu-orgw-leaf-todos-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-any-task
                               #'mugu-orgw-task-headline-p #'mugu-orgi-goto-focus-headline)
(mugu-orgi-make-headline-query mugu-orgi-goto-global-capture-headline
                               #'mugu-orgw-global-capture-headline-p #'mugu-orgi-goto-focus-headline)

(mugu-orgi-make-headline-query mugu-orgi-goto-inbox-headline
                               #'mugu-orgw-inbox-headline-p #'mugu-orgi-goto-focus-headline)

(defsubst mugu-orgi-goto-agenda-file ()
  "."
  (interactive)
  (mugu-orgi-counsel-agenda-files #'find-file))
(defsubst mugu-orgi-refile-refilable-headline () "."
  (interactive)
  (mugu-orgi-counsel-headlines #'mugu-orgw-list-refilable-headlines #'mugu-orgu-action-headline-refile))

;;;###autoload (autoload 'mugu-orgi-menu-global "mugu-org-interface.el" nil t)
(defmenu mugu-orgi-menu-global (:color blue :hint nil)
  "Org mode external interface"
  ("aa" (mugu-orgw-agenda-global) "global agenda" :column "Agenda")
  ("ct" (mugu-orgw-capture-todo #'mugu-orgi-goto-global-capture-headline) "capture todo task" :column "Capture")
  ("ca" (mugu-orgw-capture-todo #'mugu-orgi-goto-active-task) "capture todo to active task")
  ("cp" (mugu-orgw-capture-todo #'mugu-orgi-goto-project-task) "capture todo to project")
  ("ci" (mugu-orgw-capture-todo #'mugu-orgi-goto-inbox-headline) "capture todo to inbox")
  ("cn" (mugu-orgw-capture-note (mugu-orgi-counsel-agenda-files)) "capture note")
  ("rr" (mugu-orgi-refile-refilable-task) "refile refilable" :column "Refile")
  ("gfa" (mugu-orgi-goto-agenda-file) "goto agenda files" :column "Goto File")
  ("gff" (switch-to-buffer (mugu-orgu-get-last-buffer-name)) "goto last visited")
  ("ga" (mugu-orgi-goto-any-task) "goto task (any)" :column "Goto Task")
  ("gt" (mugu-orgi-goto-any-todo) "goto task (leaf")
  ("gg" (mugu-orgi-goto-leaf-headline) "goto any leaf headline")
  ("gw" (mugu-orgi-goto-wait-task) "goto wait")
  ("gpp" (mugu-orgi-goto-project-task) "goto any project" :column "Goto Project")
  ("gpt" (mugu-orgi-goto-top-project-task) "goto a top project")
  ("gpl" (mugu-orgi-goto-leaf-project-task) "goto a leaf project")
  ("gps" (mugu-orgi-goto-stuck-project-task) "goto a stuck project"))
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
  ("s" (progn (mugu-orgu-sort-subtree 'mugu-orgw-sort-cmp-headlines)
              (mugu-orgi-menu-org-major-mode)) "entry sort" :color blue)
  ("S" (progn (mugu-orgu-sort-subtree 'mugu-orgw-sort-cmp-headlines 'recursive)
              (mugu-orgi-menu-org-major-mode)) "recursive subtree sort" :color blue)
  ("a" org-archive-subtree "archive subtree")
  ("/" org-sparse-tree "search" :color blue :column "Search")
  ("q" mugu-orgi-menu-org-major-mode "Return to org menu" :color blue :column nil))

(defmenu mugu-orgi-submenu-goto
  (:color blue :hint nil :inherit (mugu-orgi-menu-hjkl-hydra/heads))
  "Submenu gathering bindings for goto query."
  ("a" (mugu-orgi-goto-active-task-local) "goto active" :column "Goto Task")
  ("t" (mugu-orgi-goto-todo-task-local) "goto todo")
  ("g" (mugu-orgi-goto-any-task-local) "goto any")
  ("n" (mugu-orgi-goto-next-task-local) "goto next")
  ("pp" (mugu-orgi-goto-project-task-local) "goto any project" :column "Goto Project")
  ("pt" (mugu-orgi-goto-top-project-task-local) "goto a top project")
  ("pl" (mugu-orgi-goto-leaf-project-task-local) "goto a leaf project")
  ("ps" (mugu-orgi-goto-stuck-project-task-local) "goto a stuck project")
  ("q" mugu-orgi-menu-org-major-mode "Return to org menu" :color blue :column nil))

(defmenu mugu-orgi-menu-org-major-mode
  (:color blue :hint nil :inherit (mugu-orgi-menu-hjkl-hydra/heads))
  "bindings for ORG mode"
  ("e" (mugu-orgi-submenu-tree-actions) "(expert) tree manipulation" :column "Sub-menus")
  ("g" (mugu-orgi-submenu-goto) "goto task/project")
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
  ("o" org-insert-todo-heading "insert todo")
  ("q" nil "exit"))

(defun mugu-orgi-set-configuration ()
  "Set org config value relative to interface."
  (setq org-use-fast-todo-selection t)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-indented t)
  ;;Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit 't)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks nil)
  (add-to-list 'display-buffer-alist
               '("CAPTURE*"
                 (display-buffer-in-side-window display-buffer-same-window display-buffer-use-some-window)
                 (side . bottom)
                 (slot . 1)
                 (window-height . 0.3)
                 (inhibit-switch-frame . t)))
  (add-to-list 'display-buffer-alist
               '(" \\*Org todo\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 2)
                 (inhibit-switch-frame . t)))
  (add-to-list 'display-buffer-alist
               '("\\*Org Agenda\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (inhibit-switch-frame . t)
                 (inhibit-same-window . t)))
  (set-face-attribute 'org-todo nil :foreground "#ff3333" :background nil)
  (advice-add #'org-switch-to-buffer-other-window :around #'mugu-orgi-switch-to-buffer-other-window))

(defun mugu-orgi-activate-menus ()
  "Activate org major mode and global menus."
  (add-hook 'org-agenda-mode-hook #'mugu-orgi-menu-agenda-major-mode)
  (mugu-menu-register-mode-menu 'org-agenda-mode 'mugu-orgi-menu-agenda-major-mode)
  (mugu-menu-register-mode-menu 'org-mode 'mugu-orgi-menu-org-major-mode))

(defun mugu-orgi-configure-keys ()
  "Gather keys binding in one place."
  ;; (setq org-agenda-ignore-drawer-properties '(effort appt category))
  (define-key org-read-date-minibuffer-local-map (kbd "M-l")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-h")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (general-def org-mode-map
    "M-j" 'org-move-subtree-down
    "M-k" 'org-move-subtree-up
    "M-h" 'org-do-promote
    "M-l" 'org-do-demote
    "M-g" 'org-promote-subtree
    "M-m" 'org-demote-subtree))


(provide 'mugu-org-interface)
;;; mugu-org-interface ends here
