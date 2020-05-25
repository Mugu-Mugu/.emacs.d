;;; mugu-org-sql --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

(require 'org-sql)
(require 'dash)
(require 'async)
(require 's)
;;; Code:

;; (defconst org-sql--schemas
;;   '("CREATE TABLE files (file_path TEXT PRIMARY KEY ASC,md5 TEXT NOT NULL,size INTEGER NOT NULL,time_modified INTEGER,time_created INTEGER,time_accessed INTEGER);"
;;     "CREATE TABLE headlines (file_path TEXT, headline_offset INTEGER, tree_path TEXT, headline_text TEXT NOT NULL, keyword TEXT, effort INTEGER, priority CHAR, archived BOOLEAN, commented BOOLEAN, content TEXT, PRIMARY KEY (file_path ASC, headline_offset ASC), FOREIGN KEY (file_path) REFERENCES files (file_path) ON UPDATE CASCADE ON DELETE CASCADE);"
;;     "CREATE TABLE tags (file_path TEXT,headline_offset INTEGER,tag TEXT,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path, headline_offset, tag, inherited));"
;;     "CREATE TABLE properties (file_path TEXT,headline_offset INTEGER,property_offset INTEGER,key_text TEXT NOT NULL,val_text TEXT NOT NULL,inherited BOOLEAN,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, property_offset ASC));"
;;     "CREATE TABLE clocking (file_path TEXT,headline_offset INTEGER,clock_offset INTEGER,time_start INTEGER,time_end INTEGER,clock_note TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset)ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, clock_offset ASC));"
;;     "CREATE TABLE logbook (file_path TEXT,headline_offset INTEGER,entry_offset INTEGER,entry_type TEXT,time_logged INTEGER,header TEXT,note TEXT,FOREIGN KEY (file_path, headline_offset)REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
;;     "CREATE TABLE state_changes (file_path TEXT,entry_offset INTEGER,state_old TEXT NOT NULL,state_new TEXT NOT NULL,FOREIGN KEY (file_path, entry_offset) REFERENCES logbook (file_path, entry_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, entry_offset ASC));"
;;     "CREATE TABLE planning_changes (file_path TEXT, entry_offset INTEGER, timestamp_offset INTEGER NOT NULL, FOREIGN KEY (file_path, entry_offset) REFERENCES logbook (file_path, entry_offset) ON DELETE CASCADE ON UPDATE CASCADE, PRIMARY KEY (file_path ASC, entry_offset ASC), FOREIGN KEY (file_path, timestamp_offset) REFERENCES timestamp (file_path, timestamp_offset) ON DELETE CASCADE ON UPDATE CASCADE);"
;;     "CREATE TABLE links (file_path TEXT,headline_offset INTEGER,link_offset INTEGER,link_path TEXT,link_text TEXT,link_type TEXT,FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON UPDATE CASCADE ON DELETE CASCADE,PRIMARY KEY (file_path ASC, link_offset ASC));"
;;     "CREATE TABLE timestamp (file_path TEXT, headline_offset INTEGER, timestamp_offset INTEGER, raw_value TEXT NOT NULL, type TEXT, planning_type TEXT, warning_type TEXT, warning_value INTEGER, warning_unit TEXT, repeat_type TEXT, repeat_value INTEGER, repeat_unit TEXT, time INTEGER NOT NULL, time_end INTEGER, resolution TEXT, resolution_end TEXT, PRIMARY KEY (file_path, timestamp_offset), FOREIGN KEY (file_path, headline_offset) REFERENCES headlines (file_path, headline_offset) ON DELETE CASCADE ON UPDATE CASCADE);")
;;   "Table schemas for the org database.")

(defconst mugu-org-sql-all-headlines-view
  '("all_headlines" . "CREATE VIEW all_headlines AS
SELECT h.file_path, h.headline_offset, h.headline_text, h.tree_path, h.keyword, h.archived, h.priority,
       GROUP_CONCAT(CASE WHEN tags.inherited = 0 THEN tags.tag END) as native_tags,
       GROUP_CONCAT(CASE WHEN tags.inherited = 1 THEN tags.tag END) as herited_tags,
       MAX(CASE WHEN c.planning_type = 'closed' THEN COALESCE(c.time_end, c.time) END) AS closed_time,
       MAX(CASE WHEN c.planning_type = 'scheduled' and c.type = 'active' THEN COALESCE(c.time_end, c.time) END) AS scheduled_time,
       MAX(CASE WHEN c.planning_type = 'deadline' and c.type = 'active' THEN COALESCE(c.time_end, c.time) END) AS deadline_time,
       CASE WHEN h.keyword = 'NEXT' THEN 1
            WHEN h.keyword = 'TODO' THEN 2
            WHEN h.keyword = 'WAIT' THEN 3
            WHEN h.keyword = 'DONE' THEN 4
            ELSE 5 END as todo_rank
FROM headlines h
LEFT OUTER JOIN tags on tags.file_path = h.file_path and tags.headline_offset = h.headline_offset
LEFT OUTER JOIN timestamp c on c.file_path = h.file_path and c.headline_offset = h.headline_offset
WHERE h.archived = 0
GROUP BY h.file_path, h.headline_text;")
  "My base view.")

(defconst mugu-org-sql-all-tasks-view
  '("all_tasks" . "CREATE VIEW all_tasks AS
SELECT *, COALESCE(scheduled_time, deadline_time) as planned_time
FROM all_headlines
WHERE keyword IS NOT NULL
ORDER BY todo_rank,
         COALESCE(planned_time, strftime('%s','now', '2 day')),
         COALESCE(priority, 'Z'),
         headline_offset;")
  "All tasks including done.")

(defconst mugu-org-sql-all-todos-view
  '("todo_tasks" . "CREATE VIEW todo_tasks as
SELECT * FROM all_tasks
WHERE keyword != 'DONE' AND keyword != 'WAIT';")
  "All tasks not done.  Todos are ordered.")

(defconst mugu-org-sql-planned-view
  '("planned_tasks" . "CREATE view planned_tasks AS
SELECT * FROM todo_tasks
WHERE planned_time IS NOT NULL;")
  "A query to select all tasks with a planned time.")

(defconst mugu-org-sql-active-view
  '("active_tasks" . "CREATE view active_tasks AS
SELECT * FROM todo_tasks
WHERE scheduled_time < CAST(strftime('%s', 'now', '1 day') AS INTEGER)
   OR deadline_time < CAST(strftime('%s', 'now', '4 day') AS INTEGER)
   OR keyword = 'NEXT';")
  "A query to select all tasks with a planned time in the past or near now or with a next TODO.")

(defconst mugu-org-sql-inbox-view
  '("inbox_tasks" . "CREATE VIEW inbox_tasks as
SELECT * from all_tasks
WHERE keyword in ('TODO', 'WAIT')
  AND COALESCE(deadline_time, scheduled_time, priority) IS NULL;
")
  "All todos without any scheduling/priority informations.  Those are considered as inbox.")

(defconst mugu-org-sql-backlog-view
  '("backlog_tasks" . "CREATE VIEW backlog_tasks as
SELECT * from all_tasks
WHERE keyword = 'TODO' and COALESCE(scheduled_time, deadline_time) IS NULL;")
  "All tasks that have no scheduled date but were prioritized.")

(defconst mugu-org-sql-reminder-view
  '("reminder_tasks" . "CREATE VIEW reminder_tasks as
SELECT *, COALESCE(deadline_time, scheduled_time, 0) AS reminder_time, CAST(strftime('%s', 'now') AS INTEGER) as now_time
FROM all_tasks
WHERE keyword = 'WAIT' and reminder_time < now_time
ORDER by reminder_time;"))


(defconst mugu-org-sql-base-flat-cols
  '(:file_path :headline_offset :headline_text :tree_path :herited_tags :native_tags :closed_time :scheduled_time :deadline_time :keyword :priority)
  "A list of columns corresponding to the base view.")

(defconst mugu-org-sql-custom-views
  (list mugu-org-sql-all-headlines-view
        mugu-org-sql-all-tasks-view
        mugu-org-sql-all-todos-view
        mugu-org-sql-active-view
        mugu-org-sql-planned-view
        mugu-org-sql-inbox-view
        mugu-org-sql-backlog-view
        mugu-org-sql-reminder-view)
  "A list of all of my custom views.")

(defvar mugu-org-sql-db-updated-hook
  nil
  "A hook indicating a db update is over.")

(defun mugu-org-sql-install-views ()
  "Install my custom views in the org sql schemas."
  (--map (progn
           (org-sql-cmd (format "DROP VIEW IF EXISTS %s;" (car it)))
           (->> (cdr it) (s-replace "\n" " ") (org-sql-cmd)))
         mugu-org-sql-custom-views))

(defun mugu-org-sql-headline-repr (p-headline)
  "Return a string representation of P-HEADLINE."
  (format "[%s] [#%s] %s %s"
          (or (s-presence (plist-get p-headline :keyword)) "NONE")
          (or (s-presence (plist-get p-headline :priority)) "#")
          (--> (plist-get p-headline :file_path)
               (file-name-base it)
               (s-capitalize it)
               (s-concat it (plist-get p-headline :tree_path))
               (s-concat it "/" (plist-get p-headline :headline_text))
               (truncate-string-to-width it 150 0 ?\ ))
          (--> (plist-get p-headline :herited_tags)
               (s-concat it (plist-get p-headline :native_tags))
               (when (s-presence it) (s-concat "@" it))
               (or it "")
               (s-replace "," "/@" it)
               (truncate-string-to-width it 60 0 ?\ ))))

(defun mugu-org-sql-action-goto (p-headline)
  "Go to the location of P-HEADLINE."
  (find-file (plist-get p-headline :file_path))
  (goto-char (string-to-number (plist-get p-headline :headline_offset))))

(defun mugu-org-sql-rfloc (p-headline)
  "Extract the rfloc of a P-HEADLINE."
  (list nil (plist-get p-headline :file_path) nil (string-to-number (plist-get p-headline :headline_offset))))

(defun mugu-org-sql-make-action (action-fun)
  "Transform standard ACTION-FUN into an action that can be applied to a P-HEADLINE."
  (lambda (p-headline)
    (let* ((org-file (plist-get p-headline :file_path))
           (org-buffer (if org-file
                           (or (find-buffer-visiting org-file) (find-file-noselect org-file))
                         (current-buffer)))
           (org-point (string-to-number (plist-get p-headline :headline_offset))))
      (with-current-buffer org-buffer
        (save-excursion
          (goto-char org-point)
          (call-interactively action-fun)
          (save-buffer))))))

(defun mugu-org-sql-select-from (view)
  "Select headlines from VIEW."
  (org-sql-cmd-select view mugu-org-sql-base-flat-cols))

(defun mugu-org-sql--activate ()
  "Run initilisation to prepare the mode."
  (org-sql-cmd-open-connection)
  (mugu-org-sql-install-views)
  (mugu-org-sql-sync-db-bg))

(defun mugu-org-sql-sync-db-bg ()
  "Sync the db if needed in background."
  (when (eq major-mode 'org-mode)
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`load-path\\'")
        (require 'org-sql)
        ,(async-inject-variables "\\`org-sql-files\\'")
        ,(async-inject-variables "\\\`org-todo-keywords\\'")
        (require 's)
        (org-sql-cmd-open-connection)
        (org-sql-user-update)
        t)
     (lambda (_)
       (run-hooks #'mugu-org-sql-db-updated-hook)))))

(define-minor-mode mugu-org-sql-mode
  "Track all orgs files and cache their content in a sqlite db."
  :global t
  (if mugu-org-sql-mode
      (progn
        (mugu-org-sql--activate)
        (add-hook 'after-save-hook #'mugu-org-sql-sync-db-bg))
    (remove-hook 'after-save-hook #'mugu-org-sql-sync-db-bg)))

(provide 'mugu-org-sql)
;;; mugu-org-sql ends here
