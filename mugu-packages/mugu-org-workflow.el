;;; mugu-org-workflow --- Summary
;; A collection of defun and settings to implement my workflow
;;; Commentary:
;; The workflow is inspired by GTD and bernst carsten one and as such mix
;; elements from both.

;; The workflow makes use of several project files, each describing a distinct
;; category.  Creation of a project file is done when a previous one becomes too
;; big or when there is really a new standalone project.
;; There is thus no dilemna as to how/when subcategorize: either it's obvious or
;; there was no other choice anyway to keep things manageable.

;; Each project file has the following :
;; - a datetree headline which serves as journaling as well as a tool to make
;;   review easier
;; - a standard hierarchical todo outline
;; - an inbox tree for quick capturing
;; - several capture template dynamically created to create journal note or
;;   capture todo easily
;; - an automatic agenda view automatically created which displays a view on in
;;   progress tasks, an overview on the potential next tasks (random quick
;;   actions) or tasks that have been explicitly selected, an overview on the
;;   project schedule/deadline and lastly a backlog of all tasks in the project

;; In addition, there is a global agenda view which gives high level overview on
;; all tasks are in progress or due today and in the near future.  In addition,
;; it gives an overview of all projects file.
;; It does not go further in details as priority and determination of next
;; action should be done locally in a project.
;; Consistency is lazily enforced by habit review whose periodicity are tailored
;; for the project requirements

;;; Code:
(require 'mugu-org-utils)
(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-habit)


(defun mugu-org-workflow/-make-capture-binding ()
  "Build capture bindings for all registered org-file.
Bindings are done if global property MUGU-LABEL is present.
4 bindings are performed : capture immediate, regular, quick todo and journal note."
  (let ((base-capture-template '(("t" "Capture a TODO entry")
                                 ("p" "Capture a IN_PROGRESS entry")
                                 ("f" "Capture a FAST_TODO entry")
                                 ("j" "Capture a note")))
        (func-cap-cmd-< (lambda (it other)
                          "comparator based on binding of a capture cmd"
                          (string< (-first-item it) (-first-item other))))
        (func-gen-todo (lambda (org-file todo-state todo-hotkey)
                         "generate a todo capture spec"
                         `(,(format "%s%s" todo-hotkey (mugu-org-utils/get-file-hotkey org-file))
                           ,(format "Capture a %s entry for %s" todo-state (file-name-base org-file))
                           entry
                           (file+headline ,org-file "Inbox")
                           ,(format "* %s %%i %%?" todo-state))))
        (func-gen-note (lambda (org-file)
                         "generate a note capture spec"
                         `(,(concat "j" (mugu-org-utils/get-file-hotkey org-file))
                           ,(concat "Capture note for " (file-name-sans-extension org-file))
                           entry
                           (file+datetree ,org-file)
                           "* %U %i %?"))))
    (-sort func-cap-cmd-<
           (apply 'append
                  base-capture-template
                  (-map (lambda (org-file)
                          (when (mugu-org-utils/get-file-hotkey org-file)
                            `(,(funcall func-gen-todo org-file "IN_PROGRESS" "p")
                              ,(funcall func-gen-todo org-file "FAST_TODO" "f")
                              ,(funcall func-gen-todo org-file "TODO" "t")
                              ,(funcall func-gen-note org-file))))
                        (org-agenda-files))))))

(defun mugu-org-workflow/-active-parent-with-active-childs? ()
  "Return t if the current entry is active and at at least 1 active child."
  (and (org-entry-is-todo-p)
       (string= "IN_PROGRESS" (org-get-todo-state))
       (> (length (org-map-entries nil "/!IN_PROGRESS" 'tree)) 1)))

(defun mugu-org-workflow/-skip-active-parent-with-active-child ()
  "A skip function for org agenda search to ignore redondant active todo."
  (when (mugu-org-workflow/-active-parent-with-active-childs?)
    (save-excursion (outline-next-heading))))

(defun mugu-org-workflow/-skip-inbox-headline ()
  "A skip function for org agenda to ignore the inbox headline container."
  (when (string= "Inbox" (-fifth-item (org-heading-components)))
    (save-excursion (outline-next-heading))))

(defun mugu-org-workflow/-skip-todo-parent-with-todo-child ()
  "A skip function self explonatory."
  (when (and (org-entry-is-todo-p)
             (not (org-entry-is-done-p))
             (> (length (org-map-entries nil "/!-DONE-CANCELED" 'tree)) 1))
    (save-excursion (outline-next-heading))))

(defun mugu-org-workflow/-make-project-overview-cmd (binding filename)
  "Make a org agenda custom command entry for on BINDING for FILENAME.
This will define a standard block agenda under the prefix p"
  (let ((the-file (file-name-base filename)))
    `(,(format "p%s" binding)
      ,(format "an overview for project %s" the-file)
      ((tags-todo "SCHEDULED=\"<today>\"|DEADLINE=\"<today>\"|TODO=\"IN_PROGRESS\""
                  ((org-agenda-overriding-header ,(format "Task in progress for project %s" the-file))
                   (org-agenda-prefix-format " %b")
                   (org-agenda-skip-function #'mugu-org-workflow/-skip-active-parent-with-active-child)))
       (tags "REFILE+LEVEL>2"
             ((org-agenda-overriding-header ,(format "Task to refile for project %s" the-file))
              (org-agenda-prefix-format " %b")))
       (agenda ""
               ((org-agenda-overriding-header ,(format "Agenda for project %s" the-file))
                (org-agenda-prefix-format " %s %b")
                (org-agenda-show-all-dates nil)
                (org-agenda-ndays 21)))
       (todo "TODO|REVIEW|NEXT|SOMEDAY"
             ((org-agenda-overriding-header ,(format "Backlog of the project %s" the-file))
              (org-agenda-prefix-format " %b")
              (org-agenda-skip-function #'mugu-org-workflow/-skip-todo-parent-with-todo-child)
              (org-agenda-sorting-strategy '(todo-state-down priority-down)))))
      ((org-agenda-files '(,filename))))))

(defconst mugu-org-workflow/global-overview-agenda
  `(("o" "Global overview of all projects"
     ((tags-todo "SCHEDULED=\"<today>\"|DEADLINE=\"<today>\"|TODO=\"IN_PROGRESS\""
            ((org-agenda-overriding-header "ALL tasks in progress or due today")
             (org-agenda-prefix-format "%-14:c %b")
             (org-agenda-skip-function #'mugu-org-workflow/-skip-active-parent-with-active-child)))
      (agenda ""
              ((org-agenda-overriding-header "Global agenda")
               (org-agenda-prefix-format " %s %-14:c %b")
               (org-agenda-show-all-dates t)
               (org-agenda-ndays 14)))
      (todo "NEXT"
            ((org-agenda-overriding-header "Next actions ready")
             (org-agenda-prefix-format "%-14:c %b")))
      (todo "FAST_TODO"
            ((org-agenda-overriding-header "Quick TODOs")
             (org-agenda-prefix-format "%-14:c %b")))))))

(defun mugu-org-workflow/goto-progress-task ()
  "Goto any headline with PROGRESS status."
  (interactive)
  (mugu-org-utils/query-entries #'mugu-org-utils/goto-headline
                                "SCHEDULED=\"<today>\"|DEADLINE=\"<today>\"|TODO=\"IN_PROGRESS\""
                                'agenda
                                #'mugu-org-workflow/-skip-active-parent-with-active-child))

(defun mugu-org-workflow/refile-task ()
  "Goto any headline with REFILE tag."
  (interactive)
  (mugu-org-utils/query-entries #'mugu-org-utils/refile-headline
                                "REFILE+LEVEL>1"
                                'agenda))

(defun mugu-org-workflow/activate ()
  "Activate the workflow.
Will modify several key variables of Org mode and create dynamic bindings for
each project file."
  (interactive)
  (push 'org-habit org-modules)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 80)
  (setq org-agenda-custom-commands
        (append mugu-org-workflow/global-overview-agenda
               (--map
                (mugu-org-workflow/-make-project-overview-cmd (mugu-org-utils/get-file-hotkey it) it)
                (org-agenda-files))))
  (setq org-capture-templates (mugu-org-workflow/-make-capture-binding))
  (setq org-todo-keywords (quote ((sequence "SOMEDAY(s)" "TODO(t)" "REVIEW(r)" "NEXT(n)"
                                            "IN_PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")
                                  (sequence "FAST_TODO(f)" "|" "DONE(d)")))))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow ends here
