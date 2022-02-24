;;; mugu-org-workflow --- Mode for my org workflow -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'org-ql)
(require 'org-roam)

(defun mugu-orgw-roam-get-daily-location ()
  "Return the filepath to the current daily note."
  (save-window-excursion
    (org-roam-dailies-goto-today "d")
    (buffer-file-name)))

(defun mugu-orgw-agenda-files-with-daily ()
  "Return the current agenda file set + today."
  (-concat (org-agenda-files)
           (list (mugu-orgw-roam-get-daily-location)) (list)))

(defun mugu-orgw-goto-planification-note ()
  "Visit the note where my planification worfklow is layed out."
  (interactive)
  (find-file "~/org/roam/20220223132236-planification_avec_orgmode.org"))

(defun mugu-orgw-view-active-tasks ()
  "Return the current active tasks in agenda and in today daily."
  (interactive)
  (org-ql-search
    (mugu-orgw-agenda-files-with-daily)
    '(and (todo)
          (or (todo "ACTIVE")
              (scheduled)
              (deadline)))
    :title "Day planner"
    :sort '(priority date todo)
    :super-groups '((:name "Overdue"
                          :scheduled past
                          :deadline past)
                   (:name "Active"
                          :todo "ACTIVE"
                          :scheduled today
                          :deadline today)
                   (:discard (:anything t)))))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow.el ends here
