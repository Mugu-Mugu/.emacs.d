;;; mugu-org-workflow --- Mode for my org workflow -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'org-ql)
(require 'org-ql-view)
(require 'org-roam)
(require 's)
(require 'mugu-window)

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

(defun mugu-orgw-goto-setupfile ()
  "Go to the global setupfile for orgmode.
It is much nicer to configure org within org."
  (interactive)
  (find-file "~/org/roam/20220223132236-planification_avec_orgmode.org"))

(defun mugu-orgw-active-view-buffers ()
  "."
  (--select (s-starts-with? org-ql-view-buffer-name-prefix (buffer-name it)) (buffer-list)))

(defun mugu-orgw-view-active-tasks ()
  "Return the current active tasks in agenda and in today daily."
  (interactive)
  (org-ql-search
    (mugu-orgw-agenda-files-with-daily)
    '(and (todo)
          (or (todo "ACTIVE")
              (scheduled)
              (deadline)))
    :title "Active tasks"
    :sort '(priority date todo)
    :super-groups '((:name "Overdue"
                          :scheduled past
                          :deadline past)
                   (:name "Active"
                          :todo "ACTIVE"
                          :scheduled today
                          :deadline today)
                   (:discard (:anything t)))))

;; (completing-read
;;  "select"
;;   (org-ql-select (mugu-orgw-agenda-files-with-daily)
;;     `(and (tags-local "inbox"))))
;; :action (byte-compile
   ;;          (lambda ()
   ;;            (let ((e (org-element-headline-parser (line-end-position))))
   ;;              (cons (org-element-property :raw-value e) e))))))

(defun mugu-orgw-view-overview (&optional tag)
  "Generate a search displaying tasks matching TAG."
  (let* ((searched-tags (or (and tag (list tag))
                            '("work" "dotfiles" "perso")))
         (tags (-concat searched-tags '("daily")))
         (title (if tag (format "Overview for %s" tag)
                  "Global overview")))
    (org-ql-search
      (mugu-orgw-agenda-files-with-daily)
      `(and (tags ,@tags)
            (or (and (todo "TODO") (not (ancestors (todo))))
                (and (todo "TODO") (planning))
                (and (todo) (not (todo "TODO")))))
      :sort '(priority date todo)
      :super-groups '((:name "Overdue" :scheduled past :deadline past)
                      (:name "Active" :todo "ACTIVE" :scheduled today :deadline today)
                      (:name "Active" :todo "ACTIVE")
                      (:name "Next" :todo "NEXT")
                      (:name "Wait" :todo "WAIT")
                      (:name "Backlog" :and
                             (:todo "TODO" :priority>= "F")
                             :and
                             (:date t :priority>= "F"))
                      (:name "Icebox" :todo "TODO")
                      (:discard (:anything t)))
      :title title)))

'(defun mugu-orgw--after-refile (&rest args)
  "."
  (org-save-all-org-buffers))

(defun mugu-orgw--activate ()
  "Setup for mugu-orgw-mode."
  (advice-add 'org-refile :after #'mugu-orgw--after-refile))

(defun mugu-orgw--deactivate ()
  "Tear down for mugu-orgw-mode."
  (advice-remove 'org-refile  #'mugu-orgw--after-refile))

(define-minor-mode mugu-orgw-mode
  "Define mugu-orgw-mode."
  :global t
  :group 'mugu
  :keymap (make-sparse-keymap)
  (if mugu-orgw-mode
      (mugu-orgw--activate)
    (mugu-orgw--deactivate)))

(provide 'mugu-org-workflow)
;;; mugu-org-workflow.el ends here
