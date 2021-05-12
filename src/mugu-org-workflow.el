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
;; * Headlines predicate
(defun mugu-orgw-private-headline-p (headline)
  "Reject HEADLINE with private tag."
  (-contains? (mugu-orgu-get-tags headline t) "private"))

(defun mugu-orgw-work-headline-p (headline)
  "Reject HEADLINE with work tag."
  (-contains? (mugu-orgu-get-tags headline t) "work"))

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

(defun mugu-orgw-action-p (headline)
  "Predicate determining if HEADLINE is an action (a todo headline)."
  (org-element-property :todo-type headline))

(defun mugu-orgw-task-p (headline)
  "Predicate determining if HEADLINE is a task.  It is a top level action."
  (and (mugu-orgw-action-p headline)
       (not (mugu-orgu-has-parent-p #'mugu-orgw-action-p headline))))

(defun mugu-orgw-wait-p (headline)
  "Predicate determining if HEADLINE is waiting."
  (equal (org-element-property :todo-keyword headline) "WAIT"))

(defun mugu-orgw-closed-p (headline)
  "Predicate determining if HEADLINE is a DONE."
  (equal (org-element-property :todo-keyword headline) "DONE"))

(defun mugu-orgw-todo-p (headline)
  "Predicate determining if HEADLINE is a TODO."
  (equal (org-element-property :todo-keyword headline) "TODO"))

(defun mugu-orgw-scheduled-p (headline)
  "Predicated determining if HEADLINE has a scheduled date and is still active."
  (and (not (mugu-orgw-closed-p headline))
       (mugu-orgw-scheduled-date headline)))

(defun mugu-orgw-done-date (headline)
  "Return the done date of HEADLINE if any."
  (mugu-orgu-timestamp-to-float (org-element-property :closed headline)))

(defun mugu-orgw-scheduled-date (headline)
  "Return the schedule date of HEADLINE if any."
  (mugu-orgu-timestamp-to-float (org-element-property :scheduled headline)))

(defun mugu-orgw-deadline-date (headline)
  "Return the deadline date of HEADLINE if any."
  (mugu-orgu-timestamp-to-float (org-element-property :deadline headline)))

(defun mugu-orgw-with-tag-p (tag headline)
  "Predicate indicating if TAG is present in HEADLINE."
  (mugu-orgu-has-tag? headline tag))

(defun mugu-orgw-schedulable-p (headline)
  "Determine if a HEADLINE is schedulable.
A HEADLINE is schedulable if at all conditions are met:
- it is either a task (not in icebox) or a scheduled todo
- it has no scheduled/deadline children"
  (and
   (not (mugu-orgw-closed-p headline))
   (or (and (not (mugu-orgw-in-icebox-p headline)) (mugu-orgw-task-p headline))
       (and (mugu-orgw-action-p headline) (mugu-orgw-scheduled-p headline)))
   (not (mugu-orgu-has-child-p headline #'mugu-orgw-scheduled-p))))

(defun mugu-orgw-done-yesterday-p (headline)
  "Predicate indicating if HEADLINE was done yesterday."
  (let ((yesterday (round (- (float-time) (mod (float-time) (* 24 3600)) (* 24 3600))))
        (done-date (mugu-orgw-done-date headline)))
    (and done-date (> done-date yesterday))))

;; * Headlines sort
(defun mugu-orgw--cmp-score-scheduled (headline time)
  "Score HEADLINE according to scheduled property.
TIME will be frozen and will be used if HEADLINE does not define it.
The earliest one is the most prioritary."
  (let* ((scheduled-date (or (mugu-orgw-scheduled-date headline) time))
         (scheduled-in-future (and scheduled-date (> scheduled-date time)))
         (icebox-task-in-future (and scheduled-in-future (mugu-orgw-in-icebox-p headline))))
    (unless icebox-task-in-future
      (and (mugu-orgw-action-p headline)
           (not icebox-task-in-future)
           scheduled-date
           (- scheduled-date)))))

(defun mugu-orgw--cmp-score-todo-state (headline)
  "Score HEADLINE according to todo state."
  (pcase (org-element-property :todo-keyword headline)
    ("ACTIVE" 7)
    ("NEXT" 6)
    ("TODO" 5)
    ("WAIT" 4)
    ("DONE" 3)
    ("STOP" 2)
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

(defun mugu-orgw--cmp-score-fast-p (headline)
  "Fast HEADLINE should be treated first."
  (if (mugu-orgw-fast-task-p headline)
      100
    -100))

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
    (if (eq (round result) 0)
        nil
      result)))

(defun mugu-orgw-cmp-headlines (hl-left hl-right)
  "Return non-nil if HL-LEFT is more prioritary than HL-RIGHT."
  (let* ((result (or (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-todo-state hl-left hl-right)
                     (mugu-orgw--cmp-headlines #'mugu-orgw--cmp-score-priority hl-left hl-right)
                     0)))
    (< result 0)))

;; * Capture
(defun mugu-orgw-capture-todo (find-loc-find &optional capture-form)
  "Capture a todo headline and store it in the headline selected by FIND-LOC-FIND.
CAPTURE-FORM is to define a custom capture template.
The hack with noflet is to prevent fucking orgmode to sabotage the windows configuration."
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let* ((capture-form (or capture-form "* TODO %i %?"))
           (org-capture-templates `(("x" "capture a task todo"
                                     entry (function ,find-loc-find) ,capture-form
                                     :unnarrowed t))))
      (org-capture nil "x"))))

(defun mugu-orgw-capture-link (find-loc-find)
  "Capture a link in the form of a todo at location selected by FIND-LOC-FIND."
  (mugu-orgw-capture-todo find-loc-find "* TODO %i %? \n %l"))

(defun mugu-orgw-checkout-headline-to-dailies (daily-capture-function)
  "Command to capture the headline at point as link to a daily.
The daily file is the one referenced by DAILY-CAPTURE-FUNCTION."
  (interactive)
  (call-interactively #'org-store-link)
  (let ((org-roam-dailies-capture-templates '(("d" "default" entry #'org-roam-capture--get-point "* TODO %a" :file-name "daily/%<%Y-%m-%d>" :head "#+title: %<%Y-%m-%d>\n" :olp
                                               ("Inbox")
                                               :unnarrowed t
                                               :immediate-finish t))))
    (call-interactively daily-capture-function)))

(defun mugu-orgw-move-headline-to-dailies (daily-goto-function)
  "Command to move the headline at point to target dailies file.
The daily file is the one referenced by DAILY-GOTO-FUNCTION."
  (interactive)
  (let ((daily-rfloc (save-window-excursion
                       (funcall daily-goto-function)
                       (mugu-orgu-rfloc-at-point))))
    (org-refile nil nil daily-rfloc)
    (save-buffer)))

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
(defun mugu-orgw-snooze (headline time)
  "Snooze the given HEADLINE until given TIME."
  (interactive (list (mugu-orgu-element-at-point) 0))
  (mugu-orgu-set-deadline headline nil)
  (mugu-orgu-schedule headline (float-time time)))

(defun mugu-orgw-set-active (headline)
  "Set HEADLINE active by scheduling it to now and changing its status to NEXT."
  (interactive (list (mugu-orgu-element-at-point)))
  (mugu-orgu-change-todo-state headline "NEXT")
  (mugu-orgu-schedule headline (float-time)))

(defun mugu-orgw-delete-timestamp (headline)
  "Reset timestamps on HEADLINE."
  (mugu-orgu-set-deadline headline nil)
  (mugu-orgu-schedule headline nil))

(defun mugu-orgw-after-todo-state-change ()
  "Triggers action after todo state change.
On WAIT ask for a new scheduled time."
  (when (and (equal (org-get-todo-state) "WAIT"))
    (call-interactively #'org-schedule)
    (call-interactively #'org-add-note)))

;; * Misc
(defun mugu-orgw-sort-tasks ()
  "Sort the arborescence by todo then priorities."
  (interactive)
  (mugu-orgu-sort-subtree #'mugu-orgw-cmp-headlines)
  (org-cycle)
  (org-cycle))

(defun mugu-orgw-set-configuration ()
  "Activate the workflow.
Will modify several key variables of Org mode and create dynamic bindings for
each project file."
  (push 'org-habit org-modules)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "ACTIVE(a)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "STOP(s@)"))))
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 80)
  (setq org-lowest-priority ?F)
  (setq org-agenda-files `(,(expand-file-name "~/org/") ,(expand-file-name "~/org/roam")))
  (setq calendar-week-start-day 1)
  (add-hook 'org-after-todo-state-change-hook #'mugu-orgw-after-todo-state-change)
  (add-hook 'org-after-refile-insert-hook #'save-buffer)
  (org-mode-restart))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow ends here
