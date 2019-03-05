;; -*- lexical-binding: t -*-
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
;; * Requirement
(require 'mugu-org-utils)
(require 'dash)
(require 'ht)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-habit)

;; * Headlines predicate
(defun mugu-orgw-leaf-headline-p (headline)
  "Predicate determining if HEADLINE is leaf."
  (not (mugu-orgu-headline-get-childs headline)))

(defun mugu-orgw-refilable-headline-p (headline)
  "Predicate determining if HEADLINE is refilable."
  (-contains? (mugu-orgu-get-tags headline 'inherit 'inherit-only)
              "refile"))

(defun mugu-orgw-inbox-headline-p (headline)
  "Predicate determining if HEADLINE is a inbox."
  (-intersection (mugu-orgu-get-tags headline)
                 '("quicky" "refile")))

(defun mugu-orgw-global-capture-headline-p (headline)
  "Predicate determining if HEADLINE is a global capture targer.
Either a inbox, a project or an active task."
  (or (mugu-orgw-inbox-headline-p headline)
      (mugu-orgw-active-headline-p headline)
      (mugu-orgw-project-headline-p headline)))

(defun mugu-orgw-active-headline-p (headline)
  "Predicate determining if HEADLINE is active.
Only leaf headline are considered."
  (and (equal (org-element-property :todo-keyword headline) "ACTIVE")
       (not (mugu-orgu-headline-has-child-with-todo-keywords headline '("ACTIVE")))))

(defun mugu-orgw-wait-headline-p (headline)
  "Predicate determining if HEADLINE is waiting."
  (equal (org-element-property :todo-keyword headline) "WAIT"))

(defun mugu-orgw-todo-headline-p (headline)
  "Predicate determining if HEADLINE is active.
Only leaf headline are considered."
  (and (equal (org-element-property :todo-keyword headline) "TODO")
       (not (mugu-orgu-headline-has-child-with-todo-keywords headline '("NEXT" "ACTIVE" "TODO")))))

(defun mugu-orgw-next-headline-p (headline)
  "Predicate determining if HEADLINE is a next step.
Only leaf headline are considered."
  (and (equal (org-element-property :todo-keyword headline) "NEXT")
       (not (mugu-orgu-headline-has-child-with-todo-keywords headline '("NEXT" "ACTIVE")))))

(defun mugu-orgw-project-headline-p (headline)
  "Predicate determining if HEADLINE is a project.
A project is just a todo headline with a child todo.  This match all project,
regardless of their nest level."
  (and (equal (org-element-property :todo-type headline) 'todo)
       (mugu-orgu-headline-has-child-with-todos headline)))

(defun mugu-orgw-top-project-headline-p (headline)
  "Predicate determining if HEADLINE is a top project.
Such a headline has no parent which is also a TODO headline."
  (and (mugu-orgw-project-headline-p headline)
       (not (mugu-orgu-headline-has-parent-with-todos? headline))))

(defun mugu-orgw-leaf-project-headline-p (headline)
  "Predicate determining if HEADLINE is a leaf project.
Such a headline is a project with no child project."
  (and (mugu-orgw-project-headline-p headline)
       (--none? (mugu-orgw-project-headline-p it) (mugu-orgu-headline-get-childs headline))))

(defun mugu-orgw-task-headline-p (headline)
  "Predicate determining if HEADLINE is a task."
  (org-element-property :todo-type headline))

(defun mugu-orgw-todos-headline-p (headline)
  "Predicate determining if HEADLINE is a task."
  (eq 'todo (org-element-property :todo-type headline)))

(defun mugu-orgw-leaf-todos-headline-p (headline)
  "Predicate determining if HEADLINE is a task."
  (and (mugu-orgw-todos-headline-p headline)
       (-none? 'mugu-orgw-todos-headline-p (mugu-orgu-headline-get-childs headline))))

(defun mugu-orgw-stuck-project-headline-p (headline)
  "Predicate determining if HEADLINE is a stuck project.
Such a headline is a project with no active or next child."
  (and (mugu-orgw-project-headline-p headline)
       (not (mugu-orgu-headline-has-child-with-todo-keywords headline '("ACTIVE" "NEXT")))))

(defun mugu-orgw-todo-rank (todo-keyword)
  "Return an integer mapping a TODO-KEYWORD to its sort rank."
  (pcase todo-keyword
    ("ACTIVE" 4)
    ("NEXT" 2)
    ("TODO" 1)
    ("WAIT" -1)
    ("DONE" -100)
    ("CANCELLED" -200)
    (_ -100)))

(defun mugu-orgw-lineage-todo-rank (todo-keyword)
  "Return an integer mapping a parent TODO-KEYWORD to its lineage sort rank."
  (pcase todo-keyword
    ("ACTIVE" 2)
    ("NEXT" 1)
    ("TODO" 0)
    ("WAIT" -10)
    ("DONE" 0)
    ("CANCELLED" 0)
    (_ 0)))

(defun mugu-orgw-get-lineage-todo-score (headline &optional print-message)
  "Return the aggregated todo score of the HEADLINE.
It takes into account todo lineage.
If PRINT-MESSAGE is true, print message instead."
  (interactive (list (mugu-orgu-element-at-point) 'print))
  (let* ((result (-sum
                  (--map
                   (mugu-orgw-lineage-todo-rank (org-element-property :todo-keyword it))
                   (mugu-orgu-lineage-todos headline)))))
    (if print-message
        (message "Lineage todo score: %s" result)
      result)))

(defun mugu-orgw-sort-get-score-headline (headline &optional print-message)
  "Return a integer value representing the sorting priority of a given entry.
Consider HEADLINE if it is provided otherwise look for element at point.
Sorted by Todo types where active one are more prioritary and then by priority
property.
If PRINT-MESSAGE is true, print message instead."
  (interactive (list (mugu-orgu-element-at-point) 'print))
  (let* ((priority-score (- 100 (mugu-orgu-get-priority headline)))
         (quicky-bonus (if (mugu-orgu-has-tag? headline "quicky" 'inherit)
                           (if (org-element-property :todo-type headline) 1000 40000000)
                         0))
         (todo-score (* 1000 (mugu-orgw-todo-rank (org-element-property :todo-keyword headline))))
         (todo-lineage-score (* 1000 (mugu-orgw-get-lineage-todo-score headline)))
         (final-score (+ todo-score todo-lineage-score quicky-bonus priority-score)))
    (if print-message
        (message "Score headline : %s [todo %s, lineage %s, priority %s]" final-score todo-score todo-lineage-score priority-score)
      final-score)))

(defun mugu-orgw-sort-cmp-headlines (hl-left hl-right)
  "Relation order between HL-LEFT and HL-RIGHT based on sorting priority."
  (> (mugu-orgw-sort-get-score-headline hl-left)
     (mugu-orgw-sort-get-score-headline hl-right)))

(defun mugu-orgw-capture-todo (find-loc-find)
  "Capture a todo headline and store it in the headline selected by FIND-LOC-FIND."
  (let ((org-capture-templates `(("x" "capture a task todo"
                                  entry (function ,find-loc-find) "* TODO %i %?"))))
    (org-capture nil "x")))

(defun mugu-orgw-capture-note (file)
  "Build a capture template for a note type headline and store it into FILE."
  (let ((org-capture-templates `(("x" "capture a note"
                                  entry (file+datetree ,file) "* %U %i %?"))))
    (org-capture nil "x")))

(defun mugu-orgw-agenda-global ()
  "Display a global org agenda view."
  (let ((org-agenda-custom-commands
         `(("o"
            "Global overview of all projects"
            ((todo ""
                   ((org-agenda-overriding-header "ALL tasks in progress or due today")
                    (org-agenda-prefix-format "%-10c | %b")
                    (org-agenda-skip-function (mugu-orgu-make-skip-function
                                               #'mugu-orgw-active-headline-p))))
             (agenda ""
                     ((org-agenda-overriding-header "Global agenda")
                      (org-agenda-prefix-format " %-10c | %-12s | %b")
                      (org-agenda-show-all-dates t)
                      (org-agenda-ndays 14)))
             (todo ""
                   ((org-agenda-overriding-header "Next actions ready")
                    (org-agenda-skip-function (mugu-orgu-make-skip-function
                                               #'mugu-orgw-next-headline-p))
                    (org-agenda-prefix-format "%-10c | %b"))))))))

    (org-agenda nil "o")))

(defun mugu-orgw-set-configuration ()
  "Activate the workflow.
Will modify several key variables of Org mode and create dynamic bindings for
each project file."
  (push 'org-habit org-modules)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "WAIT(w)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)" "CANCELLED(c)"))))
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 80)
  (setq org-tag-persistent-alist '((:startgroup . nil)
                                   ("@transport" . nil)
                                   ("@travail" . nil)
                                   ("fast_todo" . nil)
                                   ("need_review" . nil)
                                   ("quicky" . nil)
                                   ("refile" . nil)
                                   (:endgroup . nil)))
  (setq org-lowest-priority ?F)
  (setq org-agenda-files `(,(expand-file-name "~/org/")
                           ,(expand-file-name (concat user-emacs-directory "emacs.org"))))
  (org-mode-restart))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow ends here
