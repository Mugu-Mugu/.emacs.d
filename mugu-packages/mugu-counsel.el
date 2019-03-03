;;; mugu-counsel --- Summary
;; defines various various reader method
;; reimplement async with hackish but way faster method
;;; Commentary:

;;; Code:
;; * Requirement
(require 'ivy)
(require 'counsel)

;; * Asynchronous external functions utilities
(defvar mugu-counsel-temp-file "/tmp/mugu-counsel-async-cmd"
  "Temporary file used for external async queries.")

(defun mugu-counsel-exec-process-to-file (name cmd file &optional sentinel)
  "Create a process named NAME and execute a shell CMD that output to FILE.
If SENTINEL is not nil, it's called when CMD expires."
  (make-process :name name :buffer nil :connection-type 'pipe :command (list "bash" "-c" (format "%s > %s" cmd file)) :sentinel sentinel))

(defun mugu-counsel-fzf-filter-get-candidates (user-input)
  "Filter candidates in `mugu-counsel-temp-file' with fzf according to USER-INPUT."
  (with-temp-buffer
    (buffer-disable-undo)
    (call-process-shell-command
     (format "fzf -f '%s' < %s " user-input mugu-counsel-temp-file)
     nil t nil)
    (buffer-substring-no-properties (point-min) (point-max))
    (let ((cands (split-string (buffer-string) "\n" t)))
      (if cands
          cands
        (list "no match")))))

(defun mugu-counsel-fzf-matcher (regex candidates)
  "Matcher using fzf.
REGEX CANDIDATES."
  (with-temp-file mugu-counsel-temp-file
    (insert (mapconcat 'identity candidates "\n")))
  (mugu-counsel-fzf-filter-get-candidates ivy-text))

(defun call-with-fzf-matcher (func)
  "Call FUNC with a ivy matcher set to `mugu-counsel-fzf-matcher'."
  (cl-letf (((symbol-function 'ivy-state-matcher) (lambda (_) #'mugu-counsel-fzf-matcher)))
    (funcall func)))

(defun mugu-counsel-async-external-cmd (cmd)
  "Interactivly select result from external CMD with fzf in a ivy session.
No action is applied but the selected string is returned.
Matcher is fzf (if we need an external tool, it means elisp can't do it so we
don't need to hav a choice here)."
  (let ((ivy-dynamic-exhibit-delay-ms 100))
    (counsel-require-program "bash")
    (counsel-require-program "fzf")
    (lexical-let*
        ((proc (mugu-counsel-exec-process-to-file "mugu-external-cmd" cmd mugu-counsel-temp-file))
         (stub-matcher (lambda (_regex candidates) candidates))
         refresh-timer
         (refresh-action (lambda ()
                           (setq ivy--old-text nil)
                           (ivy--queue-exhibit)))
         (refresh-after-display (lambda ()
                                  (when (timerp refresh-timer)
                                    (cancel-timer refresh-timer))
                                  (when (process-live-p proc)
                                    (setq refresh-timer (run-with-timer 0.5 nil refresh-action)))))
         (cleanup (lambda ()
                    (delete-process proc)
                    (when (timerp refresh-timer) (cancel-timer refresh-timer)))))
      (sleep-for 0.2)
      (ivy-read "Recursive find: "
                'mugu-counsel-fzf-filter-get-candidates
                :dynamic-collection t
                :update-fn refresh-after-display
                :re-builder 'identity
                :sort nil
                :matcher stub-matcher
                :require-match t
                :unwind cleanup))))

;; * Interactive find files commands
(defun mugu-counsel--fd-cmd (start-directory &optional only-f-or-d)
  "Generate a fd command string to query files and dir in START-DIRECTORY.
If ONLY-F-OR-D is nil, both file and directory are queried (fast).
if ONLY-F-OR-D is equal to 'only-file then directories are not included.
if ONLY-F-OR-D is equal to 'only-dir then files are not included."
  (let ((f-or-d (pcase only-f-or-d
                  ('only-file "--type f")
                  ('only-dir "--type d")
                  (_ ""))))
    (format "fd --hidden --follow --exclude '.git' %s '' %s" f-or-d start-directory)))

(defun mugu-counsel-find-file-recursive (&optional starting-directory)
  "Interactivly find file recursivly in STARTING-DIRECTORY or `default-directory' otherwise."
  (interactive)
  (let ((directory (or starting-directory default-directory)))
    (mugu-counsel-async-external-cmd (mugu-counsel--fd-cmd directory 'only-file))))

(defun mugu-counsel-find-dir-recursive (&optional starting-directory)
  "Interactivly find file recursivly in STARTING-DIRECTORY or `default-directory' otherwise."
  (interactive)
  (let ((directory (or starting-directory default-directory)))
    (mugu-counsel-async-external-cmd (mugu-counsel--fd-cmd directory 'only-dir))))

(defun mugu-counsel-find-anything-recursive (&optional starting-directory)
  "Interactivly find things recursivly in STARTING-DIRECTORY or `default-directory' otherwise."
  (interactive)
  (let ((directory (or starting-directory default-directory)))
    (mugu-counsel-async-external-cmd (mugu-counsel--fd-cmd directory))))

;; * Interactive grep commands
(defun mugu-counsel-super-star ()
  "A ivy swiper variant of vim famous super star."
  (interactive)
  (soo-ivy/body)
  (counsel-grep-or-swiper (thing-at-point 'symbol t)))

(defun mugu-counsel-hyper-star ()
  "A ivy swiper variant of vim famous super star.  Recursive!!"
  (interactive)
  (soo-ivy/body)
  (counsel-rg (thing-at-point 'symbol t)))

;; * Bookmark commands
;; silence byte compiler
(declare-function bookmark-location "bookmark.el")
(declare-function bookmark-all-names "bookmark.el")

(defun mugu-counsel-read-bookmark-dir ()
  "Select a directory bookmark."
  (interactive)
  (require 'cl-lib)
  (require 'bookmark)
  (bookmark-location
   (ivy-read "Select bookmark name (directory):  "
             (cl-remove-if-not
              (lambda (x) (file-directory-p (bookmark-location x)))
              (bookmark-all-names))
             :caller 'counsel-bookmark)))

(defun mugu-counsel-read-bookmark-file()
  "Select a directory bookmark."
  (interactive)
  (require 'cl-lib)
  (require 'bookmark)
  (bookmark-location
   (ivy-read "Select bookmark name (file):  "
             (cl-remove-if-not
              (lambda (x) (file-regular-p (bookmark-location x)))
              (bookmark-all-names))
             :caller 'counsel-bookmark)))

;; * End
(provide 'mugu-counsel)
;;; mugu-counsel ends here
