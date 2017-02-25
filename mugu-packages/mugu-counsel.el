;;; mugu-counsel --- Summary
;; defines various various reader method
;; reimplement async with hackish but way faster method
;;; Commentary:

;;; Code:
(require 'ivy)
(require 'counsel)

;; silence byte compiler
(declare-function bookmark-location "bookmark.el")
(declare-function bookmark-all-names "bookmark.el")

(defvar mugu-counsel-recursive-history
  nil
  "History for recursive reading.")

(defun mugu-counsel--generate-temp-file (session-name)
  "Generate the file path to the temp file for the given SESSION-NAME."
  (expand-file-name (concat temporary-file-directory (format "tmp-%s.mugu" session-name))))

(defun mugu-counsel--read-recursive (f-or-d starting-directory)
   "Read a directory or a file recursivly and asynchronously.
F-OR-D is a char f or d that determine whether a file or a dir is searched.
It defaults to f. STARTING-DIRECTORY is the directory from which the recursive
search is done.  It defaults to default directory."
   (let* ((default-directory starting-directory)
          (base-prompt "Recursive find (%d/%d")
          (cmd (format "find . -type %s -not -path '*\/.git*'" f-or-d)))
     (ivy-read "Recursive find: "
               `(lambda (unused)
                  (mugu-counsel--stream-candidates ,cmd
                                                   ,(mugu-counsel--generate-temp-file "fdr")
                                                   #'mugu-counsel--select-filter-func
                                                   t)
                  nil)
               ;; (split-string
               :dynamic-collection t
               :history 'mugu-counsel-recursive-history
               :unwind 'mugu-counsel--cleanup-candidates-stream
               :caller 'mugu-counsel-recursive-find)))

;;;###autoload
(defun mugu-counsel-read-recursive-dir (&optional starting-directory)
  "Read a directory recursivly and asynchronously.
STARTING-DIRECTORY is the directory from which the recursive search is done.
It defaults to default directory.
Return the absolute path of selected directory"
  (interactive)
  (let ((starting-directory
         (or starting-directory default-directory)))
    (expand-file-name (mugu-counsel--read-recursive "d" starting-directory) starting-directory)))


;;;###autoload
(defun mugu-counsel-read-recursive-file (&optional starting-directory)
  "Read a file recursivly and asynchronously.
STARTING-DIRECTORY is the directory from which the recursive search is done.
It defaults to default directory.
Return the absolute path of selected file"
  (interactive)
  (let ((starting-directory
         (or starting-directory default-directory)))
    (expand-file-name (mugu-counsel--read-recursive "f" starting-directory) starting-directory)))

;;;###autoload
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

;;;###autoload
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

(defun mugu-counsel--cleanup-candidates-stream ()
"Ensure proper cleanup of an ivy session with `mugu-counsel--stream-candidates'."
  (counsel-delete-process)
  (kill-buffer " *counsel*")
  (swiper--cleanup))

 (defun mugu-counsel--stream-candidates (cmd temp-file-name filter-func &optional initial-only)
  "Run shell CMD asynchronously to generate candidates for ivy.
This function is part of a chain of functions managing the candidate streaming.
This one manages the shell CMD that generates the raw candidates.
CMD output is forwarded to TEMP-FILE-NAME.
FILTER-FUNC is expected to use content from TEMP-FILE-NAME to populate ivy
completion buffer dynamically. It will be called as long as there is still
unread data.
INITIAL-ONLY is a boolean that indicates if the candidates are fully dynamic or
if only the first batch is async.  If nil, CMD is run only once during the
session. Otherwise, CMD is cancelled and recalled at each function invocation.
Caller should ensure that a ivy-session is properly finalized with
`mugu-counsel--cleaup-candidates-stream.'"
   (let* ((counsel--process " *counsel*")
          (proc (get-process counsel--process))
          (buff (get-buffer counsel--process)))
     (if initial-only
         (unless (bufferp buff)
           (setq proc (start-process-shell-command
                       counsel--process
                       counsel--process
                       (concat cmd " > " temp-file-name))))
       (when proc
         (delete-process proc))
       (when buff
         (kill-buffer buff))
       (setq proc (start-process-shell-command
                   counsel--process
                   counsel--process
                   (concat cmd " > " temp-file-name))))
     (run-with-idle-timer 0.10 nil #'mugu-counsel--filter-stream-candidates proc temp-file-name filter-func ivy-text)))

 (defun mugu-counsel--filter-stream-candidates (proc temp-file-name filter-func ivy-input)
   "Filter candidates generated by PROC in TEMP-FILE-NAME with FILTER-FUNC.
This function is part of a chain of functions managing the candidate streaming.
The actual filtering done is delegated to FILTER-FUNC. This function just ensure
FILTER-FUNC will be called perioddically as long as PROC is alive. This is in
fine a filter/sentinel but with way better performance and responsivness."
   (when (and (equal ivy-input ivy-text)
              (minibufferp))
     (when (process-live-p proc)
       (run-with-idle-timer 0.3 nil 'mugu-counsel--filter-stream-candidates proc temp-file-name filter-func ivy-input))
     (let* ((original-prompt (ivy-prompt))
            (ivy--prompts-list nil)
            (ivy--prompt (if (process-live-p proc)
                             (concat "PENDING " original-prompt)
                           original-prompt)))
       (while-no-input
         (funcall filter-func temp-file-name)))))

 (defun mugu-counsel--filter-func (temp-file-name)
   "Extract from TEMP-FILE-NAME a list of candidates for the current ivy session.
TEMP-FILE-NAME contents are extracted as-is."
   (ivy--insert-minibuffer
    (ivy--format
     (ivy--sort-maybe (setq ivy--all-candidates
                            (with-temp-buffer
                              (buffer-disable-undo)
                              (insert-file-contents-literally temp-file-name)
                              (let ((cands (split-string (buffer-string) counsel-async-split-string-re t)))
                                (if cands
                                    cands
                                  (list "no match")))))))))

(defun mugu-counsel--select-filter-func (temp-file-name)
  "Extract from TEMP-FILE-NAME a list of candidates for the current ivy session.
TEMP-FILE-NAME contents are filtered to match `ivy-text'"
  (ivy--insert-minibuffer
   (ivy--format
    (ivy--sort-maybe (setq ivy--all-candidates
                           (with-temp-buffer
                             (buffer-disable-undo)
                             (let ((regex (counsel-unquote-regex-parens
                                           (setq ivy--old-re
                                                 (ivy--regex ivy-text)))))
                               (call-process-shell-command
                                (format "grep -iE \"%s\" %s" regex temp-file-name)
                                nil t nil))
                             (let ((cands (split-string (buffer-string) counsel-async-split-string-re t)))
                               (if cands
                                   cands
                                 (list "no match")))))))))

(defun mugu-counsel--ag-function (string base-cmd extra-ag-args)
  "Overrides default `counsel-ag-function' for a better async handling.
STRING, BASE-CMD and EXTRA-AG-ARGS have same semantic."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let* ((args-end (string-match " -- " extra-ag-args))
             (file (if args-end
                       (substring-no-properties extra-ag-args (+ args-end 3))
                     ""))
             (extra-ag-args (if args-end
                                (substring-no-properties extra-ag-args 0 args-end)
                              extra-ag-args))
             (ag-cmd (format base-cmd
                             (concat extra-ag-args

                                     (shell-quote-argument regex)
                                     file))))
        (if (file-remote-p default-directory)
            (split-string (shell-command-to-string ag-cmd) "\n" t)
          (mugu-counsel--stream-candidates ag-cmd
                                           (mugu-counsel--generate-temp-file "rg")
                                           #'mugu-counsel--filter-func)
          nil)))))
(advice-add #'counsel-ag-function :override #'mugu-counsel--ag-function)

(defun small-recentf ()
  "A small recentf wrapper to be included in buffer list."
  (defvar recentf-list)
  (require 'recentf)
  (mapcar (lambda (x)
            (concat (file-name-base x) "." (file-name-extension x)))
          (cl-subseq recentf-list 0 20)))

(ivy-set-sources 'ivy-switch-buffer '((small-recentf) (original-source)))
(defun mugu--filter-without-dups (original-filter-func name candidates)
  "Ensure ivy candidates have no duplicates (especially for multi source.
This is an advice around ivy--filter as ORIGINAL-FILTER-FUNC with same semantics
for NAME and CANDIDATES"
  (delete-dups (funcall original-filter-func name candidates)))
;; (advice-add #'ivy--filter :around #'mugu--filter-without-dups)

;;;###autoload
(defun mugu-counsel-super-star ()
  "A ivy swiper variant of vim famous super star."
  (interactive)
  (soo-ivy/body)
  (counsel-grep-or-swiper (thing-at-point 'symbol t)))

;;;###autoload
(defun mugu-counsel-hyper-star ()
  "A ivy swiper variant of vim famous super star.  Recursive!!"
  (interactive)
  (soo-ivy/body)
  (counsel-rg (thing-at-point 'symbol t)))
;; ivy--sources-list is a variable defined in ‘ivy.el’.
;; Its value is (ivy-switch-buffer
 ;; ((original-source)
  ;; (ivy-source-views)))

(provide 'mugu-counsel)
;;; mugu-counsel ends here
