;;; Package --- Summary
;;; provide reusable function related to org
;;; Commentary:
;;; most of these utils function were inspired from : Bernt Hansen

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'swiper)

(defun mugu-org-is-project-p ()
  "Any task with a todo keyword subtask."
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
  "Return t if header belongs to an active tree.
that is if all of its parent tasks are active or if it has no parent task"
  (save-restriction
    (widen)
    (save-excursion
      (while (and (org-up-heading-safe)
                  (or (not (org-get-todo-state)) (equal (org-get-todo-state) "NEXT"))))
      (or (not (org-get-todo-state))
          (equal (org-get-todo-state) "NEXT")))))

(defun mugu-org-project-stuck-p ()
  "Return t if project has a direct task which is active."
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
          (when (org-get-todo-state) (org-end-of-subtree t)))
;;; project is stuck if stop heading state is not NEXT
        (not (equal (org-get-todo-state) "NEXT"))))))

(defun mugu-org-skip-if-parent-inactive ()
  "Skip current subtree if it belongs to an inactive tree (without Next)."
  (cond ((mugu-org-task-in-active-tree-p) nil)
        (t (outline-next-heading))))

(defun mugu-org-skip-project-or-inactive-branch ()
  "Skip if current header is in inactive tree or if its a project."
  (or (mugu-org-skip-project-header) (mugu-org-skip-if-parent-inactive))
  )

(defun mugu-org-skip-project-not-stuck ()
  "Skip current headline if it's not a stuck project.
To be considered stuck, an headline shall meet the following conditions
- it is within an active tree
- it is a project
- no direct subtask is active"
  (cond ((and (mugu-org-task-in-active-tree-p) (mugu-org-is-project-p) (mugu-org-project-stuck-p)) nil)
        (t (outline-next-heading))))

(defun mugu-org-skip-project-header ()
  "Skip header that are projects."
  (cond ((mugu-org-is-project-p) (outline-next-heading))
        (t nil)))

(defun mugu-orgu/get-headline-metadata ()
  "Return a cons cell with heading data and a standard org property alist.
This alist is enriched with the point of the heading in the file."
  (interactive)
  (let ((heading (concat (mapconcat 'identity (org-get-outline-path) " > ") "> " (org-get-heading)))
        (metadata-alist (org-entry-properties)))
    (push (cons "POINT" (point)) metadata-alist)
    `(,heading . ,metadata-alist)))

(defun mugu-org-utils/query-entries (action-function &optional match scope &rest skip)
  "Select an entry returned by `org-map-entries' and apply ACTION-FUNCTION.
MATCH SCOPE and SKIP are forwarded as is to `org-map-entries'.  The entries
match result are forwarded to an ivy session where user can select on which
entry ACTION-FUNCTION will be applied.  ACTION-FUNCTION must be a function that
accept a `org-entry-properties' alist.  This alist is enriched with 2 properties
: FILE and POINT."
  (ivy-read
   "Select a headline : "
   (apply #'org-map-entries #'mugu-orgu/get-headline-metadata match scope skip)
   :action (lambda (selected-headline) (funcall action-function selected-headline))))

(defun mugu-org-utils/get-headline-prop (headline property)
  "Return from HEADLINE the value of PROPERTY.
HEADLINE is as returned by `mugu-orgu/get-headline-metadata'"
  (cdr (assoc (symbol-name property) (cdr headline))))

(defun mugu-org-utils/goto-headline (headline)
  "Goto HEADLINE, and focus on it by minimizing all other.
HEADLINE format is expected to be as returned by
`mugu-orgu/get-headline-metadata' and should provide a file and a point."
  (find-file (mugu-org-utils/get-headline-prop headline 'FILE))
  (goto-char (mugu-org-utils/get-headline-prop headline 'POINT))
  (org-shifttab 1)
  (org-reveal)
  (org-cycle))

(defun mugu-org-utils/refile-headline (headline)
  "Refile HEADLINE at point.
HEADLINE format is expected to be as returned by
`mugu-orgu/get-headline-metadata' and should provide a file and a point."
  (let ((todo-only t))
    (save-excursion
      (find-file (mugu-org-utils/get-headline-prop headline 'FILE))
      (goto-char (mugu-org-utils/get-headline-prop headline 'POINT))
      (org-refile))))

(defun mugu-org-utils/get-file-hotkey (&optional org-file)
  "Return the label of ORG-FILE or nil if absent the current file.
The label is a global file property with key MUGU-LABEL.
It's used to generate unique capture/agenda binding from common template.
It should be a character unique to all agenda files.
If the property is not found, nil is returned."
  (save-window-excursion
    (when (file-exists-p org-file) (find-file org-file))
    (cdr (assoc (symbol-name 'MUGU-LABEL) org-file-properties))))

(defun mugu-org-utils/agenda-forward-block ()
  "Move point to the next block in agenda block.  Hackish..."
  (interactive)
  (when (ignore-errors (search-forward "======"))
    (call-interactively #'org-agenda-next-item)))

(defun mugu-org-utils/agenda-backward-block ()
  "Move point to the next block in agenda block.  Hackish..."
  (interactive)

  (if (eq (save-excursion (call-interactively #'org-agenda-goto-block-beginning)
                          (point))
          (save-excursion (call-interactively #'org-agenda-previous-item)
                          (call-interactively #'org-agenda-goto-block-beginning)
                          (point)))
      ;; we were not at the first entry of the block so just go at it now
      (progn (call-interactively #'org-agenda-goto-block-beginning)
             (call-interactively #'org-agenda-next-item))
    ;; we were at the first entry so go to begining of previous block
    (call-interactively #'org-agenda-goto-block-beginning)
    (call-interactively #'org-agenda-previous-item)
    (call-interactively #'org-agenda-goto-block-beginning)
    (call-interactively #'org-agenda-next-item)))

(provide 'mugu-org-utils)
;;; mugu-org-utils ends here
