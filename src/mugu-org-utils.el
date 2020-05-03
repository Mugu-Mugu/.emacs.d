;;; mugu-org-utils --- Summary
;; -*- lexical-binding: t -*-
;; tbc
;;; Commentary:

;;; Code:
;;; Package --- Summary
;;; provide reusable function related to org
;;; Commentary:
;;; Almost every function in this package expects an HEADLINE argument which
;;; should be an org-element object generated from `org-element-parse-buffer'. A
;;; local element obtained by `org-element-at-point' will not work as it doesn't
;;; have :parent property or children.
;;;  All headline generated by functions of this package are global and should
;;;  be accepted by every functions here.

;;; Code:

;; * Require
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'dash)
(require 'ht)
(eval-when-compile (require 'cl))

;; Getter on headlines
(defun mugu-orgu-get-file (headline)
  "Get file where HEADLINE is located."
  (org-element-property :file headline))

(defun mugu-orgu-get-tags (headline &optional inherit inherit-only)
  "Retrieve local tags of HEADLINE.
When INHERIT is non-nil also fetch tags from its parents (recursively).
When INHERIT and INHERIT-ONLY are both non-nil retrieve only tags from parents."
  (when headline
    (let* ((include-self (not (and inherit inherit-only)))
           (headlines (if inherit
                          (org-element-lineage headline nil include-self)
                        (list headline))))
      (-uniq
       (-flatten
        (-concat
         org-file-tags
         (--map (or (org-element-property :tags it)
                    (list))
                headlines)))))))

(defun mugu-orgu-get-priority (headline)
  "Retrieve the priority of the HEADLINE or the inherited one of its parents."
  (cond ((not headline) org-default-priority)
        ((and headline
              (org-element-property :priority headline)))
        ((mugu-orgu-get-priority (org-element-property :parent headline)))))

;; Actions on headlines
(defun mugu-orgu--simulate-universal-arg-for-date (time)
  "Simulate universal argument called for `org-deadline' bzsed on TIME value."
  (if time nil '(4)))

(defun mugu-orgu-put-property (headline property value)
  "Change in HEADLINE the choosen PROPERTY to a new VALUE.
Property refers to the native `org' one (not `org-element')."
  (mugu-orgu-do-action #'org-put-property headline property (format "%s" value)))

(defun mugu-orgu-delete-property (headline property)
  "Delete in HEADLINE the choosen PROPERTY.
Property refers to the native `org' one (not `org-element')."
  (mugu-orgu-do-action #'org-delete-property headline property))

(defun mugu-orgu-change-todo-state (headline &optional todo-state)
  "Change the HEADLINE TODO-STATE."
  (mugu-orgu-do-action #'org-todo headline todo-state))

(defun mugu-orgu-set-priority (headline)
  "Change the HEADLINE prirority."
  (mugu-orgu-do-action #'org-priority headline))

(defun mugu-orgu-do-action (action-function headline &rest args)
  "Apply ACTION-FUNCTION to HEADLINE or headline at point if it's nil.
ARGS are applied as is to ACTION-FUNCTION."
  (let* ((org-file (mugu-orgu-get-file headline))
         (org-buffer (if org-file
                         (or (find-buffer-visiting org-file) (find-file-noselect org-file))
                       (current-buffer)))
         (org-point (or (org-element-property :begin headline) (point))))
    (with-current-buffer org-buffer
      (save-excursion
        (goto-char org-point)
        (apply action-function args)))))

(defun mugu-orgu-schedule (headline time)
  "Change the schedule time of HEADLINE to TIME."
  (mugu-orgu-do-action #'org-schedule headline
                       (mugu-orgu--simulate-universal-arg-for-date time)
                       (mugu-orgu-time-to-timestamp time)))

(defun mugu-orgu-set-deadline (headline time)
  "Change the deadline time of HEADLINE to TIME."
  (mugu-orgu-do-action #'org-deadline headline
                       (mugu-orgu--simulate-universal-arg-for-date time)
                       (mugu-orgu-time-to-timestamp time)))

(defun mugu-orgu--refile-to (target-headline)
  "Refile headline at point to TARGET-HEADLINE."
  (save-restriction
    (org-narrow-to-element)
    (let ((rfloc (list (org-element-property :raw-value target-headline)
                       (mugu-orgu-get-file target-headline)
                       nil
                       (org-element-property :begin target-headline))))
      (org-refile nil nil rfloc))))

(defun mugu-orgu-action-headline-refile (headline target-headline)
  "Refile HEADLINE to TARGET-HEADLINE.
HEADLINE is a an org-element object generated from any mugu-orgu function."
  (mugu-orgu-do-action #'mugu-orgu--refile-to headline target-headline))

(defun mugu-orgu-action-headline-copy (headline target-headline)
  "Copy HEADLINE to TARGET-HEADLINE.
HEADLINE is a an org-element object generated from any mugu-orgu function."
  (let ((org-refile-keep t))
    (mugu-orgu-do-action #'mugu-orgu--refile-to headline target-headline)))

;; Time utilities
(defun mugu-orgu-timestamp-to-float (org-timestamp)
  "Convert a ORG-TIMESTAMP to a number of seconds since epoch."
  (and org-timestamp (float-time (org-timestamp-to-time org-timestamp))))

(defun mugu-orgu-time-to-timestamp (time)
  "Convert a TIME to a timestamp understood by org mode.
TIME must be homogenous to `float-time'."
  (org-timestamp-format (org-timestamp-from-time time 'with-time)
                        (org-time-stamp-format 'long)))

;; General headlines predicates
(defun mugu-orgu-all-p (&rest predicates-and-headline)
  "Predicate determining if headline match all given predicates.
PREDICATES-AND-HEADLINE should be a list of predicates following by an headline."
  (let ((predicates (-butlast predicates-and-headline))
        (headline (-last-item predicates-and-headline)))
    (--all? (funcall it headline) predicates)))

(defun mugu-orgu-todo-headline-p (headline)
  "Predicicate for HEADLINE indicating if it's a TODO."
  (and (org-element-property :todo-type headline)
       headline))

(defun mugu-orgu-has-parent-p (parent-predicate headline)
  "Predicate indicating if HEADLINE has any parent meeting PARENT-PREDICATE."
  (-first-item (-filter parent-predicate (org-element-lineage headline))))

(defun mugu-orgu-has-child-p (headline child-predicate)
  "Predicate indicating if HEADLINE has any child meeting CHILD-PREDICATE."
  (let ((child-predicate-ignore-self (lambda (hl)
                                       (and (not (eq hl headline))
                                            (funcall child-predicate hl)))))
    (org-element-map headline 'headline child-predicate-ignore-self nil 'first-match)))

(defun mugu-orgu--position-visible-p (position)
  "Predicate indicting if POSITION is visible or not in current buffer."
  (and (<= position (point-max))
       (<= (point-min) position)))

(defun mugu-orgu-action-headline-goto (headline)
  "Goto HEADLINE.
HEADLINE is a an org-element object generated from any mugu-orgu function."
  (let* ((headline-point (org-element-property :begin headline))
         (headline-filename (mugu-orgu-get-file headline)))
    (find-file-noselect headline-filename)
    (mugu-buffer-switch (get-file-buffer headline-filename))
    (unless (mugu-orgu--position-visible-p headline-point)
      (widen))
    (goto-char headline-point)))

(defun mugu-orgu-has-tag? (headline tag &optional inherit inherit-only)
  "Predicate indicating if HEADLINE contain TAG.
When INHERIT is non-nil also fetch tags from its parents (recursively).
When INHERIT and INHERIT-ONLY are both non-nil retrieve only tags from parents."
  (-contains? (mugu-orgu-get-tags headline inherit inherit-only) tag))

;; List headline function
(defun mugu-orgu--select-and-decorate-headline (select-headline-p)
  "Build a headline decorator function when SELECT-HEADLINE-P is fullfilled.
If predicate is not respected, nil is returned, otherwise the headline is
returned with some additional properties embedeed.
For now only :file is added but we may cache some value in custom defined
property if performance indicates."
  (lambda (headline)
    (org-element-put-property headline :file (buffer-file-name))
    (and (funcall select-headline-p headline) headline)))

(defun mugu-orgu-list-headlines-local (select-headline-p)
  "Return a list of headlines matching SELECT-HEADLINE-P in the current subtree.
SELECT-HEADLINE-P is a predicate function taking a single argument: the
org-element object related to a candidate headline."
  (save-restriction
    (org-narrow-to-subtree)
    (org-element-map (org-element-parse-buffer 'headline 'visible-only) 'headline
      (mugu-orgu--select-and-decorate-headline select-headline-p))))

(defun mugu-orgu-list-headlines-in-file (file select-headline-p)
  "Return a list of headline in FILE satisfying SELECT-HEADLINE-P condition.
Returned headlines are org-element format with a :file property added containing
file path to the headline as well as a :outline property which is an
aggreagation of all parents headline description."
  (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
    (save-restriction
      (widen)
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (mugu-orgu--select-and-decorate-headline select-headline-p)))))

(defun mugu-orgu-list-headlines-in-same-file (select-headline-p headline)
  "Return a list of headlines satisfying SELECT-HEADLINE-P.
Only the headlines in the same file of HEADLINE will be selected."
  (or (mugu-orgu-list-headlines-in-file (mugu-orgu-get-file headline) select-headline-p)
      (list)))

(defun mugu-orgu-list-headlines (select-headline-p)
  "Return a list of headlines satisfying SELECT-HEADLINE-P.
SELECT-HEADLINE-P is a predicate function taking a single argument: the
org-element object related to a candidate headline.
Returned headlines are org-element format with a :file property added containing
file path to the headline as well as a :outline property which is an
aggreagation of all parents headline description."
  (--mapcat (mugu-orgu-list-headlines-in-file it select-headline-p) (org-agenda-files)))

(defun mugu-orgu-list-childs (headline select-headline-p &optional with-parent)
  "Return all child headlines of HEADLINE matching SELECT-HEADLINE-P condition.
If WITH-PARENT is non nil, also include HEADLINE."
  (let* ((parent-filename (mugu-orgu-get-file headline))
         (select-and-append-filename
          (lambda (headline)
            (when (funcall select-headline-p headline)
              (org-element-put-property headline :file parent-filename))))
         (child-headlines (org-element-map headline 'headline select-and-append-filename)))
    (if with-parent
        child-headlines
      (--remove-first (eq headline it) child-headlines))))

;; Agenda functions
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

;; Misc function
(defun mugu-orgu-get-last-buffer-name ()
  "Return the name of the last visited org buffer.
If no org buffer was visited return scratch"
  (--first (string-match-p ".*.org$" it)
           (--map (buffer-name it) (buffer-list))))

(defmacro with-cached-org-element (&rest body)
  "Evaluate BODY as if `org-element-at-point' was global."
  `(lexical-let*
       ((get-file-headlines (lambda (file)
                              (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
                                (org-element-map (org-element-parse-buffer 'headline) 'headline
                                  (lambda (hl)
                                    (org-element-put-property hl :file (buffer-file-name))
                                    (cons (format "%s%s" buffer-file-truename (org-element-property :begin hl))
                                          hl))))))
        (all-file-headlines-alist (-mapcat get-file-headlines (org-agenda-files)))
        (headlines-map (ht<-alist all-file-headlines-alist)))
     (cl-labels ((org-element-at-point () (ht-get headlines-map (format "%s%s" buffer-file-truename (point)))))
       (progn ,@body))))

(defun mugu-orgu-element-at-point ()
  "Return element at point with full content and lineage."
  (with-cached-org-element
   (save-excursion
     (beginning-of-line)
     (org-element-at-point))))

(defun mugu-orgu-sort-subtree (cmp-fun &optional recursive)
  "Sort the childs of headline at point according to order defined in CMP-FUN.
CMP-FUN should accept two org-element headline as inputs and should return
non-nil result if the first one should sort before the first.
If RECURSIVE is non-nil, the sort is performed for every descendant."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-show-subtree)
      (let* ((ast (org-element-parse-buffer 'object 'visible-only))
             (get-parent-point (lambda (hl)
                                 (or (org-element-property :begin (org-element-property :parent hl))
                                     0)))
             (cmp-by-parent-point (lambda (hl1 hl2)
                                    (< (funcall get-parent-point hl1)
                                       (funcall get-parent-point hl2))))
             (root-hl (org-element-at-point))
             (headline-selector (lambda (hl)
                                  (when (or recursive
                                            (<= (org-element-property :level hl)
                                                (+ 1 (org-element-property :level root-hl))))
                                    hl)))
             (headlines (-sort cmp-by-parent-point (org-element-map ast 'headline headline-selector)))
             (hl-same-parent (--partition-by (org-element-property :begin
                                                                   (org-element-property :parent it))
                                             headlines)))
        (-each hl-same-parent
          (lambda (hls)
            (let ((parent (org-element-property :parent (-first-item hls)))
                  (sorted-hls (-sort cmp-fun hls)))
              (--each hls (org-element-extract-element it))
              (apply 'org-element-adopt-elements parent sorted-hls))))
        (delete-region (point-min) (point-max))
        (insert (org-element-interpret-data ast))))
    (kill-line)))

(defun mugu-orgu-make-skip-function (select-headline-p)
  "Build a skip function selecting only headlines satisfying SELECT-HEADLINE-P.
The actual query given to `org-agenda' should not be more restrictive than the
equivalent one generated from SELECT-HEADLINE-P."
  (lexical-let* ((current-file nil)
                 (headline-points nil)
                 (select-headline-p select-headline-p))
    (lambda ()
      (unless (equal current-file (buffer-file-name))
        (setq current-file (buffer-file-name))
        (setq headline-points
              (-concat (--map (org-element-property :begin it)
                              (mugu-orgu-list-headlines-in-file current-file select-headline-p))
                       `(,(point-max)))))
      (while (> (point) (-first-item headline-points))
        (pop headline-points))
      (when (< (point) (-first-item headline-points))
        (-first-item headline-points)))))

(provide 'mugu-org-utils)
;;; mugu-org-utils ends here
