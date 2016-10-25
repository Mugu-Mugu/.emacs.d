;;; provide reusable function related to org
;;; most of these utils function were inspired from : Bernt Hansen

(require 'org)

(defun mugu-org-is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun mugu-org-task-in-active-tree-p ()
  "return t if header belongs to an active tree: 
that is if all of its parent tasks are active or if it has no parent task"
  (save-restriction
    (widen)
    (save-excursion
      (while (and (org-up-heading-safe)
                  (or (not (org-get-todo-state)) (equal (org-get-todo-state) "NEXT"))))
      (or (not (org-get-todo-state))
          (equal (org-get-todo-state) "NEXT")))))

(defun mugu-org-project-stuck-p ()
  "return t if project has a direct task which is active"
  (save-restriction
    (widen)
    (save-excursion
      (let ((end-of-tree-point (save-excursion (org-end-of-subtree t))))
        ;;; as long as there are still next heading
        ;;; and provided we are still in the subtree of current project
        ;;; iterate over headings and stop only if an active task as been found
        ;;; when an inactive task is encountered, its subtree is skipped
        (while (and (save-excursion (outline-next-heading) (< (point) end-of-tree-point))
                    (outline-next-heading)
                    (not (equal (org-get-todo-state) "NEXT")))
          ;; skip subtree of inactive task
          (when (org-get-todo-state)
            (org-end-of-subtree t)
            (forward-line 1)))
      ;;; project is stuck if stop heading state is not NEXT
      (not (equal (org-get-todo-state) "NEXT"))))))

(defun mugu-org-skip-if-parent-inactive ()
  "Skip current subtree if it belongs to an inactive tree (without Next)"
  (cond ((mugu-org-task-in-active-tree-p) nil)
        (t (outline-next-heading))))

(defun mugu-org-skip-project-or-inactive-branch ()
  "Skip if current header is in inactive tree or if its a project"
  (or (mugu-org-skip-project-header) (mugu-org-skip-if-parent-inactive))
  )

(defun mugu-org-skip-project-not-stuck ()
  "Skip current headline if it's not a stuck project. To be considered stuck, an headline shall meet the following conditions
- it is within an active tree
- it is a project
- no direct subtask is active "
  (cond ((and (mugu-org-task-in-active-tree-p) (mugu-org-is-project-p) (mugu-org-project-stuck-p)) nil)
        (t (outline-next-heading))))

(defun mugu-org-skip-project-header ()
  "Skip header that are projects"
  (cond ((mugu-org-is-project-p) (outline-next-heading))
        (t nil)))

(provide 'mugu-org-utils)
