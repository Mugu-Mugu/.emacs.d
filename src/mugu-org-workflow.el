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
  (and (mugu-orgw-task-p headline)
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

(defun mugu-orgw-wait-p (headline)
  "Predicate determining if HEADLINE is waiting."
  (equal (org-element-property :todo-keyword headline) "WAIT"))

(defun mugu-orgw-task-p (headline)
  "Predicate determining if HEADLINE is a task (has any todo)."
  (org-element-property :todo-type headline))

(defun mugu-orgw-todo-p (headline)
  "Predicate determining if HEADLINE is a TODO."
  (equal (org-element-property :todo-keyword headline) "TODO"))

(defun mugu-orgw-scheduled-task-p (headline)
  "Predicate determining if HEADLINE is a scheduled task."
  (and (mugu-orgw-todo-p headline) (mugu-orgw-has-todo-date-p headline)))

(defun mugu-orgw-has-todo-date-p (headline)
  "Predicated determining if HEADLINE has a scheduled or deadline date."
  (or (mugu-orgw-scheduled-date headline) (mugu-orgw-deadline-date headline)))

(defun mugu-orgw-scheduled-date (headline)
  "Return the schedule date of HEADLINE if any."
  (mugu-orgu-timestamp-to-float (org-element-property :scheduled headline)))

(defun mugu-orgw-deadline-date (headline)
  "Return the deadline date of HEADLINE if any."
  (mugu-orgu-timestamp-to-float (org-element-property :deadline headline)))

(defun mugu-orgw-schedulable-task-p (headline)
  "Determine if a HEADLINE is schedulable.
A HEADLINE is schedulable if all conditions are met:
- it is a task
- it has a scheduled/deadline entry or has no TODO parent
- it has no scheduled/deadline children"
  (and
   (mugu-orgw-task-p headline)
   (or (mugu-orgw-has-todo-date-p headline) (not (mugu-orgu-has-parent-p headline #'mugu-orgw-todo-p)))
   (not (mugu-orgu-has-child-p headline #'mugu-orgw-scheduled-task-p))))

(defun mugu-orgw-list-headlines (headline-p)
  "List headlines satisfying HEADLINE-P and not FORBIDDEN-HEADLINE-P."
  (let* ((full-headline-p (lambda (h) (and (funcall headline-p h)
                                           (not (funcall mugu-orgw-forbidden-headline-p-function h))))))
    (mugu-orgu-list-headlines full-headline-p)))

(defun mugu-orgw-with-tag-p (context-tag headline)
  "Predicate indicating if CONTEXT-TAG is present in HEADLINE."
  (mugu-orgu-has-tag? headline context-tag))

;; * Headlines sort
(defun mugu-orgw--cmp-score-deadline (headline)
    "Score HEADLINE according to deadline property.
Deadline makes sense for scheduling when it's past due.
The earliest one is the most prioritary."
    (let ((deadline-date (mugu-orgw-deadline-date headline)))
    (and (mugu-orgw-todo-p headline)
         deadline-date
         (<= deadline-date (float-time))
         (- deadline-date))))

(defun mugu-orgw--cmp-score-scheduled (headline)
  "Score HEADLINE according to scheduled property.
The earliest one is the most prioritary."
  (let ((scheduled-date (mugu-orgw-scheduled-date headline)))
    (and (mugu-orgw-todo-p headline)
         scheduled-date
         (- scheduled-date))))

(defun mugu-orgw--cmp-score-todo-state (headline)
  "Score HEADLINE according to todo state."
  (pcase (org-element-property :todo-keyword headline)
    ("TODO" 5)
    ("WAIT" 4)
    ("DONE" 3)
    ("CANCELLED" 2)
    (_ 1)))

(defun mugu-orgw--cmp-score-habit-future (headline)
  "Score HEADLINE according to habit property.
An Habit in the future should not be selected."
  (let* ((scheduled-date (mugu-orgw-scheduled-date headline))
         (scheduled-in-future (> (or scheduled-date 0) (float-time)))
         (is-habit (org-element-property :LAST_REPEAT headline)))
    (if (and is-habit scheduled-in-future)
        -1
      1)))

(defun mugu-orgw--cmp-score-priority (headline)
  "Score HEADLINE according to todo state."
  (- (mugu-orgu-get-priority headline)))

(defun mugu-orgw--cmp-headlines (score-fun hl-left hl-right)
  "Compare according to SCORE-FUN HL-LEFT and HL-RIGHT.
SCORE-FUN should be a function returning a score relative to an HEADLINE.
The higher the score the most prioritary.
Returns a integer indicating the difference or nil if headlines score are
identical."
  (let* ((default-score most-negative-fixnum)
         (score-left (or (funcall score-fun hl-left) default-score))
         (score-right (or (funcall score-fun hl-right) default-score))
         (result (- score-right score-left)))
    (if (eq result 0)
        nil
      result)))

(defun mugu-orgw-cmp-headlines (hl-left hl-right)
  "Return non-nil if HL-LEFT is more prioritary than HL-RIGHT."
  (let ((result (or (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-habit-future hl-left hl-right)
                    (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-deadline hl-left hl-right)
                    (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-scheduled hl-left hl-right)
                    (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-todo-state hl-left hl-right)
                    (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-priority hl-left hl-right)
                    0)))
    (< result 0)))

;; * Capture
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

;; * Agenda
(defun mugu-orgw-agenda-future-overview ()
  "Display a global org agenda view about upcoming events.."
  (let ((org-agenda-custom-commands
         `(("o"
            "Global overview for future activities"
            ((agenda ""
                     ((org-agenda-overriding-header "Global agenda")
                      (org-agenda-prefix-format "%-12t% s %b")
                      (org-agenda-show-all-dates t)
                      (org-agenda-use-time-grid t)
                      (org-agenda-start-day "-1d")
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-span 28)
                      (org-agenda-entry-types '(:scheduled :deadline :timestamp)))))))))
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

;; * Scheduling
(defun mugu-orgw-snooze (headline delay)
  "Snooze the given HEADLINE by given DELAY.
Its scheduled date will be set to now + the given delay."
  (interactive (list (mugu-orgu-element-at-point) 0))
  (mugu-orgu-set-deadline headline nil)
  (mugu-orgu-schedule headline (+ delay (float-time))))

(defun mugu-orgw-set-active (headline)
  "Set HEADLINE active by settings its deadline to now."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgu-set-deadline headline (float-time)))

(defun mugu-orgw-delete-timestamp (headline)
  "Reset timestamps on HEADLINE."
  (mugu-orgu-set-deadline headline nil)
  (mugu-orgu-schedule headline nil))

;; * Misc
(defun mugu-orgw-current-task ()
  "Retrieve the current active task."
  (-first-item (-sort #'mugu-orgw-cmp-headlines (mugu-orgw-list-headlines 'mugu-orgw-schedulable-task-p))))

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
  (setq calendar-week-start-day 1)
  (org-mode-restart))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow ends here
