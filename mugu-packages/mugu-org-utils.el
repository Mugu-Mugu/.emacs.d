;;; Package --- Summary
;;; provide reusable function related to org
;;; Commentary:

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'swiper)
(require 'dash)

(defun mugu-orgu/get-last-buffer-name ()
    "Return the name of the last visited org buffer.
If no org buffer was visited return scratch"
    (--first (string-match-p ".*.org$" it) (--map (buffer-name it) (buffer-list))))

(defun mugu-org-utils/my-sort ()
  "Sort tree by timestamp, priority and lastly by todo state.
Important/mature entry will be at the top of the tree."
  (interactive)
  (org-sort-entries t ?p)
  (org-sort-entries t ?s)
  (org-sort-entries t ?O))

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
