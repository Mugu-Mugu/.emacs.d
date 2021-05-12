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
  '("all_headlines" . "
CREATE VIEW all_headlines AS
SELECT h.file_path as filepath,
       h.headline_offset as offset,
       h.headline_text as title,
       h.tree_path as outline,
       h.keyword,
       h.archived,
       h.priority,
       h.rowid,
       h.content,
       tags.tag as tags,
       GROUP_CONCAT(DISTINCT CASE WHEN tags.inherited = 0 THEN tags.tag END) as native_tags,
       GROUP_CONCAT(DISTINCT CASE WHEN tags.inherited = 1 THEN tags.tag END) as herited_tags,
       MAX(CASE WHEN c.planning_type = 'closed' THEN COALESCE(c.time_end, c.time) END) AS closed_at,
       MAX(CASE WHEN c.planning_type = 'scheduled' and c.type = 'active' THEN COALESCE(c.time_end, c.time) END) AS scheduled_at,
       MAX(CASE WHEN c.planning_type = 'deadline' and c.type = 'active' THEN COALESCE(c.time_end, c.time) END) AS deadline_at
FROM headlines h
LEFT OUTER JOIN tags on tags.file_path = h.file_path and tags.headline_offset = h.headline_offset
LEFT OUTER JOIN timestamp c on c.file_path = h.file_path and c.headline_offset = h.headline_offset
WHERE h.archived = 0 AND h.file_path NOT LIKE '%org_archive'
GROUP BY h.file_path, h.headline_text;")
  "My base view.")

(defconst mugu-org-sql-planned-view
  '("planned_tasks" . "CREATE view planned_tasks AS
SELECT * FROM todo_tasks
WHERE planned_time IS NOT NULL;")
  "A query to select all tasks with a planned time.")

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

(defconst mugu-org-sql-upcoming-reminder-view
  '("upcoming_reminder_tasks" . "CREATE VIEW upcoming_reminder_tasks as
SELECT *, COALESCE(deadline_time, scheduled_time, 0) AS reminder_time, CAST(strftime('%s', 'now') AS INTEGER) as now_time
FROM all_tasks
WHERE keyword = 'WAIT' and reminder_time > now_time
ORDER by reminder_time;"))

(defconst mugu-org-sql-custom-views
  (list mugu-org-sql-all-headlines-view
        mugu-org-sql-planned-view
        mugu-org-sql-inbox-view
        mugu-org-sql-backlog-view
        mugu-org-sql-reminder-view
        mugu-org-sql-upcoming-reminder-view)
  "A list of all of my custom views.")

;; Installation
(defun mugu-org-sql-install-fts ()
  "Install the virtual FTS tables."
  (org-sql-cmd "DROP TABLE IF EXISTS headlines_fts;")
  (org-sql-cmd "CREATE VIRTUAL TABLE headlines_fts USING fts5(_file_path, _headline_offset UNINDEXED, _tree_path, _headline_text, _tags, _keyword, prefix=3);")
  (org-sql-cmd "INSERT INTO headlines_fts(_file_path, _headline_offset, _tree_path, _headline_text, _tags, _keyword) SELECT filepath, offset, outline, title, tags, keyword FROM all_headlines;"))

(defun mugu-org-sql-install-views ()
  "Install my custom views in the org sql schemas."
  (--map (progn
           (org-sql-cmd (format "DROP VIEW IF EXISTS %s;" (car it)))
           (->> (cdr it) (s-replace "\n" " ")
                (org-sql-cmd)))
         mugu-org-sql-custom-views))

(defun mugu-org-sql--activate ()
  "Run initilisation to prepare the mode."
  (org-sql-cmd-open-connection)
  (mugu-org-sql-install-views)
  (mugu-org-sql-install-fts)
  (mugu-org-sql-full-async-db-update))

;; misc
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

;; p-headline
(cl-defstruct (mugu-headline
               (:constructor nil)
               (:constructor new-mugu-headline
                             (&key
                              filepath offset title outline tags herited_tags
                              native_tags closed_at scheduled_at deadline_at keyword
                              priority
                              &aux
                              (offset (string-to-number offset))
                              (tags (s-split "," tags))
                              (herited_tags (s-split "," herited_tags))
                              (native_tags (s-split "," native_tags)))))
  (filepath :type string :documentation "Path of the file where the headline is")
  (offset :type number :documentation "Offset of the headline in the file")
  (title :type string :documentation "The headline title (first line)")
  (outline :type string :documentation "The hierarchy outline of the headline")
  (tags :type list :documentation "All applicable tags of the headline")
  (herited_tags :type list :documentation "Tags from ancestor headlines")
  (native_tags :type list :documentation "Tags at this headline")
  (closed_at :type number :documentation "Time when the headline was closed")
  (scheduled_at :type number :documentation "Time when the headline was scheduled")
  (deadline_at :type number :documentation "Time of the deadline of the headline")
  (keyword :type string :documentation "The todo class of the headline")
  (priority :type number :documentation "The priority of the headline in numeric form"))
;; les champs de celle ci va conditionner les champs qu'on demande dans la requête (ou bien est ce l'inverse?
;; il y a aussi le besoin de split les tags en une list par example

(defsubst mugu-headline-default-columns ()
  "Return the default column in `mugu-headline'."
  (->> (cl-struct-slot-info 'mugu-headline)
       (-remove-first #'identity)
       (-map #'-first-item)))

;; Selection
;;; Interface
(cl-defstruct sql-matcher "A condition")
(cl-defgeneric mugu-sql-where-clause (sql-matcher)
  "Convert a SQL-MATCHER to its SQL WHERE clause representation.
The WHERE term should not be included so it can be combined with other `sql-matcher'.")
(cl-defgeneric mugu-sql-join-clause (sql-matcher) "Convert a SQL-MATCHER to its SQL JOIN clause representation.")

;; Base implementation
(cl-defmethod mugu-sql-join-clause ((_sql-matcher sql-matcher)) "Stub condition." "")
(cl-defmethod mugu-sql-where-clause ((_sql-matcher sql-matcher)) "Stub condition." "")


;;; simple inclusion criteria
(cl-defstruct (sql-matcher-in (:include sql-matcher)
                              (:constructor sql-matcher-in-files
                                            (&rest files &aux
                                                   (criteria-name "filepath")
                                                   (criterias files)))
                              (:constructor sql-matcher-with-tags
                                            (&rest tags &aux
                                                   (criteria-name "tags")
                                                   (criterias tags))))
  (criterias :type list :documentation "The list of the criteria values")
  (criteria-name :type string :documentation "The name of the criteria"))

(cl-defmethod mugu-sql-where-clause ((sql-matcher sql-matcher-in))
  "Return SQL-MATCHER in SQL language."
  (s-concat (sql-matcher-in-criteria-name sql-matcher) " in ("
            (s-join "," (--map
                         (s-wrap it "'")
                         (sql-matcher-in-criterias sql-matcher)))
            ")"))


;;; simple matcher criteria
(cl-defstruct (sql-matcher-match (:include sql-matcher)
                                 (:constructor sql-matcher-match-title
                                               (pattern &aux
                                                        (criteria-name "title"))))
  (pattern :type string :documentation "The pattern to look for")
  (criteria-name :type string :documentation "The name of the criteria"))

 (cl-defmethod mugu-sql-where-clause ((sql-matcher sql-matcher-match))
  "Return SQL-MATCHER in SQL language."
  (s-concat (sql-matcher-match-criteria-name sql-matcher)  " LIKE " (s-wrap (sql-matcher-match-pattern sql-matcher) "'%" "%'")))


;;; fts criteria
(cl-defstruct (sql-matcher-fts (:include sql-matcher)
                            (:constructor sql-matcher-fts-standard
                                          (outline-or-tag-pattern &aux
                                                                  (pattern outline-or-tag-pattern)
                                                                  (table 'headlines_fts))))
  (pattern :type string :documentation "The FTS pattern to look for")
  (table :type string :documentation "The table where the fts information is stored"))

(cl-defmethod mugu-sql-join-clause ((sql-matcher sql-matcher-fts))
  "Return the necessary JOIN on the extra FTS virtual table.
SQL-MATCHER."
  (let ((table (sql-matcher-fts-table sql-matcher)))
    (format "INNER JOIN %s ON all_headlines.filepath = %s._file_path AND all_headlines.offset = %s._headline_offset" table table table)))

(cl-defmethod mugu-sql-where-clause ((sql-matcher sql-matcher-fts))
  "Return SQL-MATCHER in SQL language."
  (let* ((splitted-patterns (s-split " " (sql-matcher-fts-pattern sql-matcher) 'omit-nulls))
         (prefix-patterns (s-join " " (--map (s-wrap it "" "*") splitted-patterns)))
         (table (sql-matcher-fts-table sql-matcher)))
    (format "%s MATCH '%s' " table prefix-patterns)))

;; select
(defun mugu-org-sql-cmd-select (&rest conds)
  "Select columns from TBL-NAME where COLS is the list of columns.
CONDS are a list of `sql-matcher' to refine the query."
  (let* ((col-keywords (--map (intern (format ":%s" it)) (mugu-headline-default-columns)))
         (col-str (mapconcat #'symbol-name (mugu-headline-default-columns) ","))
         (tbl-str (-first-item mugu-org-sql-all-headlines-view))
         (join-statement (s-join " " (-map #'mugu-sql-join-clause conds)))
         (where-clauses (s-join " AND " (-map #'mugu-sql-where-clause conds)))
         (where-statement (if (s-present? where-clauses)
                              (concat " WHERE " where-clauses)
                            ""))
         (cmd (format "select %s from %s %s %s;" col-str tbl-str join-statement where-statement))
         (to-rows (lambda (result) (s-split "\n" result)))
         (to-plist (lambda (row) (-interleave col-keywords (s-split "|" row)))))
    (message "%s" cmd)
    (->> cmd
         (org-sql-cmd)
         (funcall to-rows)
         (-map to-plist)
         (--map (apply #'new-mugu-headline it)))))

;;; updates
(defun mugu-org-sql-sync-fts (file)
  "Sync the db fts cache for FILE."
  (org-sql-cmd (format "DELETE FROM headlines_fts where _file_path='%s';" file))
  (org-sql-cmd (format "INSERT INTO headlines_fts(_file_path, _headline_offset, _tree_path, _headline_text, _tags, _keyword) SELECT filepath, offset, outline, title, tags, keyword FROM all_headlines WHERE filepath'%s';" file)))

(defun mugu-org-sql-update-file (&optional file)
  "Sync the db with content of FILE.
If FILE is not present, will use current one."
  (when (eq major-mode 'org-mode)
    (let ((org-sql-single-file (or file buffer-file-name)))
      (org-sql-update-db)
      (mugu-org-sql-sync-fts org-sql-single-file))))

(defun mugu-org-sql-full-async-db-update ()
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
     'ignore)))

(define-minor-mode mugu-org-sql-mode
  "Track all orgs files and cache their content in a sqlite db."
  :global t
  (if mugu-org-sql-mode
      (progn
        (mugu-org-sql--activate)
        (add-hook 'after-save-hook #'mugu-org-sql-update-file))
    (remove-hook 'after-save-hook #'mugu-org-sql-update-file)))

(provide 'mugu-org-sql)
;;; mugu-org-sql ends here
