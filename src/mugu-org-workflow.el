;;; mugu-org-workflow --- Summary -*- lexical-binding: t -*-
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
;; * Requirement
(require 'mugu-org-utils)
(require 'dash)
(require 's)
(require 'ht)
(require 'noflet)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-habit)

;; variable
(defvar mugu-orgw-forbidden-headline-p-function #'mugu-orgw-private-headline-p)

;; * Headlines predicate
(defun mugu-orgw-private-headline-p (headline)
  "Reject HEADLINE with private tag."
  (-contains? (mugu-orgu-get-tags headline t) "private"))

(defun mugu-orgw-refilable-headline-p (headline)
  "Predicate determining if HEADLINE is refilable."
  (and (mugu-orgw-task-headline-p headline)
       (-contains? (mugu-orgu-get-tags headline 'inherit 'inherit-only)
                   "refile")))

(defun mugu-orgw-refilable-to-mobile-headline-p (headline)
  "Predicate determining if HEADLINE is meant to be done on transport.
Those that are meant for transport but already affected won't be selected."
  (and (not (s-contains? "mobile.org" (org-element-property :file headline)))
       (-contains? (mugu-orgu-get-tags headline) "@transport")))

(defun mugu-orgw-inbox-for-capture-headline-p (headline)
  "Predicate determining if HEADLINE is a inbox made for refile purpose."
  (-contains? (mugu-orgu-get-tags headline) "refile"))

(defun mugu-orgw-inbox-headline-p (headline)
  "Predicate determining if HEADLINE is a inbox."
  (-contains? (mugu-orgu-get-tags headline) "inbox"))

(defun mugu-orgw-active-headline-p (headline)
  "Predicate determining if HEADLINE is active."
  (eq 'todo (org-element-property :todo-type headline)))

(defun mugu-orgw-wait-headline-p (headline)
  "Predicate determining if HEADLINE is waiting."
  (equal (org-element-property :todo-keyword headline) "WAIT"))

(defun mugu-orgw-wait-or-snoozed-headline-p (headline)
  "Predicate determining if HEADLINE is waiting."
  (or (equal (org-element-property :todo-keyword headline) "WAIT")
      (> (mugu-orgw--get-scheduled headline) (float-time))))

(defun mugu-orgw-todo-headline-p (headline)
  "Predicate determining if HEADLINE is active.
Only leaf headline are considered."
  (and (equal (org-element-property :todo-keyword headline) "TODO")
       (not (mugu-orgu-headline-has-child-with-todo-keywords headline '("NEXT" "TODO")))))

(defun mugu-orgw-next-task-p (headline)
  "Predicate determining if HEADLINE is a next step.
Only leaf headline are considered."
  (and (mugu-orgw-task-headline-p headline)
       (equal (org-element-property :todo-keyword headline) "NEXT")))

(defun mugu-orgw-project-headline-p (headline)
  "Predicate determining if HEADLINE is a project.
A project is just a headline (todo or not) with a tag todo."
  (-contains? (mugu-orgu-get-tags headline) "project"))

(defun mugu-orgw-project-active-headline-p (headline)
  "Predicate determining if HEADLINE is a project.
A project is just a headline (todo or not) with a tag todo."
  (and (mugu-orgw-active-headline-p headline)
       (mugu-orgw-project-headline-p headline)))

(defun mugu-orgw-task-headline-p (headline)
  "Predicate determining if HEADLINE is a task.
A task is either:
- a top level todo HEADLINE
- or a child todo HEADLINE of a project.
It can't be both a task and a project (project takes priority)."
  (let ((parent-headline (mugu-orgu-parent-todo-headline headline)))
    (and (mugu-orgu-todo-headline-p headline)
         (not (mugu-orgw-project-headline-p headline))
         (or (not parent-headline) (mugu-orgw-project-headline-p parent-headline)))))

(defun mugu-orgw-next-task-headline-p (headline)
  "Predicate determining if HEADLINE is a next task."
  (and (mugu-orgw-task-headline-p headline)
       (equal "NEXT" (org-element-property :todo-keyword headline))))

(defun mugu-orgw-stuck-project-headline-p (headline)
  "Predicate determining if HEADLINE is a stuck project.
Such a headline is a project with no next child."
  (and (mugu-orgw-project-headline-p headline)
       (not (eq 'done (org-element-property :todo-type headline)))
       (not (mugu-orgu-headline-has-child-with-todo-keywords headline '("NEXT")))))

(defun mugu-orgw-list-headlines (headline-p)
  "List headlines satisfying HEADLINE-P and not FORBIDDEN-HEADLINE-P."
  (let* ((full-headline-p (lambda (h) (and (funcall headline-p h)
                                           (not (funcall mugu-orgw-forbidden-headline-p-function h))))))
    (mugu-orgu-list-headlines full-headline-p)))

(defun mugu-orgw--score-todo (headline)
  "Return an integer mapping the todo status of HEADLINE to its sort rank."
  (pcase (org-element-property :todo-keyword headline)
    ("NEXT" 1000)
    ("TODO" 1)
    ("WAIT" -1)
    ("DONE" -100)
    ("CANCELLED" -200)
    (_ -100)))

(defun mugu-orgw--get-scheduled (headline)
  "Return the scheduled data of HEADLINE if present.
If not present return nil."
  (float-time (org-timestamp-to-time (or (org-element-property :scheduled headline)
                                         (org-timestamp-from-time 0)))))

(defun mugu-orgw--score-scheduled (now headline)
  "Return a penalty score for HEADLINE dependant on scheduled field.
Penalty is computed relative to NOW."
  (let ((scheduled-time (mugu-orgw--get-scheduled headline)))
    (cond ((< scheduled-time 0) 0)
          ((< scheduled-time now) scheduled-time)
          ((>= scheduled-time now) (- (- scheduled-time) now)))))

(defun mugu-orgw--score-priority (headline)
  "Return a penalty score for HEADLINE dependant on last active field.
Penalty is computed relative to NOW."
  (- (or (mugu-orgu-get-priority headline) 100)))

(defun mugu-orgw--sort-cmp-score (score-fun hl-left hl-right)
  "Relation order between HL-LEFT and HL-RIGHT based on SCORE-FUN.
SCORE-FUN should be a function taking a headline in parameter and returning a
integer which correspond to the score.  The higher the score the more prioritary
the corresponding headline."
  (let ((score-left (funcall score-fun hl-left))
        (score-right (funcall score-fun hl-right)))
    (cond ((> score-left score-right) 'sup)
          ((< score-left score-right) 'inf)
          ((= score-left score-right) nil))))

(defun mugu-orgw-sort-cmp-headlines (hl-left hl-right)
  "Relation order between HL-LEFT and HL-RIGHT based on sorting priority."
  (let ((score-scheduled (apply-partially #'mugu-orgw--score-scheduled (float-time))))
    (eq 'sup (or (mugu-orgw--sort-cmp-score #'mugu-orgw--score-todo hl-left hl-right)
                 (mugu-orgw--sort-cmp-score score-scheduled hl-left hl-right)
                 (mugu-orgw--sort-cmp-score #'mugu-orgw--score-priority hl-left hl-right)))))

(defun mugu-orgw-capture-todo (find-loc-find)
  "Capture a todo headline and store it in the headline selected by FIND-LOC-FIND.
The hack with noflet is to prevent fucking orgmode to sabotage the windows configuration."
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-capture-templates `(("x" "capture a task todo"
                                    entry (function ,find-loc-find) "* TODO %i %?"))))
      (org-capture nil "x"))))

(defun mugu-orgw-capture-to-headline (headline)
  "Capture a todo and store it in the given HEADLINE as child.
The hack with noflet is to prevent fucking orgmode to sabotage the windows configuration."
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-capture-templates `(("x" "capture a task todo"
                                    entry (function ,(lambda () (mugu-orgu-action-headline-goto headline))) "* TODO %i %?"))))
      (org-capture nil "x"))))

(defun mugu-orgw-capture-note (file)
  "Build a capture template for a note type headline and store it into FILE."
  (let ((org-capture-templates `(("x" "capture a note"
                                  entry (file+datetree ,file) "* %U %i %?"))))
    (org-capture nil "x")))

(defun mugu-orgw-agenda-future-overview ()
  "Display a global org agenda view about upcoming events.."
  (let ((org-agenda-custom-commands
         `(("o"
            "Global overview for future activities"
            ((agenda ""
                     ((org-agenda-overriding-header "Global agenda")
                      ;; (org-agenda-prefix-format " %-10c | %-12s | -12t% %b")
                      (org-agenda-show-all-dates t)
                      (org-agenda-use-time-grid t)
                      (org-agenda-start-day "-1d")
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-span 28)
                      (org-agenda-entry-types '(:deadline :timestamp))))
             (todo ""
                   ((org-agenda-overriding-header "Stuck projects")
                    (org-agenda-prefix-format "%-10c | %b")
                    (org-agenda-skip-function (mugu-orgu-make-skip-function
                                               #'mugu-orgw-stuck-project-headline-p))))
             (todo ""
                   ((org-agenda-overriding-header "Tasks to refile")
                    (org-agenda-skip-function (mugu-orgu-make-skip-function
                                               #'mugu-orgw-refilable-headline-p))
                    (org-agenda-prefix-format "%-10c | %b")))
             (todo ""
                   ((org-agenda-overriding-header "Next tasks")
                    (org-agenda-skip-function (mugu-orgu-make-skip-function
                                               #'mugu-orgw-next-task-p))
                    (org-agenda-prefix-format "%-10c | %b"))))))))
    (org-agenda nil "o")))

(defun mugu-orgw-agenda-today-overview ()
  "Display a global org agenda with scheduled item today..."
  (let ((org-agenda-custom-commands
         `(("o"
            "Global overview for future activities"
            ((agenda ""
                     ((org-agenda-overriding-header "Planned today")
                      (org-agenda-show-all-dates t)
                      (org-agenda-use-time-grid t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-span 'day)
                      (org-agenda-entry-types '(:deadline :timestamp :scheduled)))))))))
    (org-agenda nil "o")))

(defun mugu-orgw-agenda-current-overview ()
  "Display a global org agenda view."
  (let* ((current-task (mugu-orgw-current-task))
         (current-project (mugu-orgw-current-project))
         (current-task-p (apply-partially #'mugu-orgu-headline-equals-p current-task))
         (current-project-p (apply-partially #'mugu-orgu-headline-equals-p current-project))
         (task-of-current-project-p (lambda (headline)
                                      (mugu-orgu-headline-equals-p current-project (mugu-orgu-get-parent headline))))
         (subtask-of-current-task-p (lambda (headline)
                                      (mugu-orgu-headline-equals-p current-task (mugu-orgu-get-parent headline))))
         (org-agenda-custom-commands `(("o"
                                        "Global overview of current activities"
                                        ((todo ""
                                               ((org-agenda-overriding-header "Current project")
                                                (org-agenda-prefix-format "%-10c | %b")
                                                (org-agenda-skip-function ,(mugu-orgu-make-skip-function current-project-p))))
                                         (todo ""
                                               ((org-agenda-overriding-header "Current task")
                                                (org-agenda-skip-function ,(mugu-orgu-make-skip-function current-task-p))
                                                (org-agenda-prefix-format "%-10c | %b")))
                                         (todo ""
                                               ((org-agenda-overriding-header "All projects")
                                                (org-agenda-skip-function ,(mugu-orgu-make-skip-function #'mugu-orgw-project-headline-p))
                                                (org-agenda-prefix-format "%-10c | %b")))
                                         (todo ""
                                               ((org-agenda-overriding-header "Tasks of current project")
                                                (org-agenda-skip-function ,(mugu-orgu-make-skip-function task-of-current-project-p))
                                                (org-agenda-prefix-format "%-10c | %b")))
                                         (todo ""
                                               ((org-agenda-overriding-header "Subtasks of current task")
                                                (org-agenda-skip-function ,(mugu-orgu-make-skip-function subtask-of-current-task-p))
                                                (org-agenda-prefix-format "%-10c | %b"))))))))

    (org-agenda nil "o")))

(defun mugu-orgw-schedule (headline &optional delay relative)
  "Set SCHEDULED field of HEADLINE to now.
If DELAY is given, add it to the timestamp.
If RELATIVE is defined, the delay is applied to the old value.  Otherwise it's
applied to now."
  (let* ((delay (or delay 0))
         (old-scheduled-raw (org-element-property :scheduled headline))
         (initial-timestamp (if (and relative old-scheduled-raw)
                                (mugu-orgw--get-scheduled headline)
                              (float-time)))
         (new-timestamp (+ delay initial-timestamp)))
    (mugu-orgu-do-action #'org-schedule
                         headline
                         'remove-old (mugu-orgu-timestamp-str new-timestamp))))

(defun mugu-orgw-current-task ()
  "Retrieve the current active task."
  (-first-item (-sort #'mugu-orgw-sort-cmp-headlines (mugu-orgw-list-headlines 'mugu-orgw-task-headline-p))))

(defun mugu-orgw-current-project ()
  "Retrieve the current active project."
  (-first-item (-sort #'mugu-orgw-sort-cmp-headlines (mugu-orgw-list-headlines 'mugu-orgw-project-headline-p))))

(defun mugu-orgw-list-subtasks (task-headline)
  "Return a list of subtask for the given TASK-HEADLINE."
  (or (mugu-orgu-headline-get-childs task-headline #'mugu-orgu-todo-headline-p) (list task-headline)))

(defun mugu-orgw-list-project-tasks (project-headline)
  "Return a list of subtask for the given PROJECT-HEADLINE."
  (or (mugu-orgu-headline-get-childs project-headline #'mugu-orgw-task-headline-p) (list project-headline)))

(defun mugu-orgw-delete-timestamp (headline)
  "Reset the timestamp of HEADLINE to now."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgu-delete-property headline "last-active"))

(defun mugu-orgw-set-task-active (headline)
  "Set task HEADLINE active.
Change it's status to NEXT and record the time at which it occured."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgu-change-todo-state headline "NEXT")
  (mugu-orgw-schedule headline))

(defun mugu-orgw-set-configuration ()
  "Activate the workflow.
Will modify several key variables of Org mode and create dynamic bindings for
each project file."
  (push 'org-habit org-modules)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "WAIT(w)" "NEXT(n)"  "|" "DONE(d)" "STOP(s)"))))
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 80)
  (setq org-tag-persistent-alist '((:startgroup . nil)
                                   ("@transport" . nil)
                                   ("@travail" . nil)
                                   ("fast_todo" . nil)
                                   ("project" . nil)
                                   ("need_review" . nil)
                                   ("quicky" . nil)
                                   ("refile" . nil)
                                   (:endgroup . nil)))
  (setq org-lowest-priority ?F)
  (setq org-agenda-files `(,(expand-file-name "~/org/")))
  (org-mode-restart))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow ends here
