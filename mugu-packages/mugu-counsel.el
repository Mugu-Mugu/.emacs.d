(require 'ivy)
(require 'counsel)

(split-string
               (shell-command-to-string "find * -type f -not -path '*\/.git*'")
               "\n" t)

(defun counsel-locate-function (input)
  (if (< (length input) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (funcall counsel-locate-cmd input))
    '("" "working...")))

(defun mugu-test (input))
(defun mugu-counsel-read-recursive-dir ()
  (interactive)
  (with-mugu-dir
   (lambda ()
     (interactive)
     (let ((mugu "mugu"))
       (start-process-shell-command mugu mugu "find / -type f -not -path '*\/.git*' > /media/sf_D_DRIVE/unsorted/counsel.temp")) (ivy-read "Recursive Dir: "
       (lambda (string)
         (message "%s" string) 
         (counsel--async-command (format "grep %s  /media/sf_D_DRIVE/unsorted/counsel.temp" string))
         nil) ;; (split-string
       :dynamic-collection t
       :history 'file-name-history
       :unwind #'counsel-delete-process
       :caller 'counsel-file-jump))))

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--exhibit post-command-hook)
    (let ((inhibit-field-text-motion nil))
      (constrain-to-field nil (point-max)))
    (setq ivy-text (ivy--input))
    (cond (ivy--directory
           (cond ((or (string= "~/" ivy-text)
                      (and (string= "~" ivy-text)
                           ivy-magic-tilde))
                  (ivy--cd (expand-file-name "~/")))
                 ((string-match "/\\'" ivy-text)
                  (ivy--magic-file-slash))))
          ((eq (ivy-state-collection ivy-last) 'internal-complete-buffer)
           (when (or (and (string-match "\\` " ivy-text)
                          (not (string-match "\\` " ivy--old-text)))
                     (and (string-match "\\` " ivy--old-text)
                          (not (string-match "\\` " ivy-text))))
             (setq ivy--all-candidates
                   (if (and (> (length ivy-text) 0)
                            (eq (aref ivy-text 0)
                                ?\ ))
                       (ivy--buffer-list " ")
                     (ivy--buffer-list "" ivy-use-virtual-buffers)))
             (setq ivy--old-re nil))))
    (ivy--insert-minibuffer
     (with-current-buffer (ivy-state-buffer ivy-last)
       (ivy--format
        (ivy--filter ivy-text ivy--all-candidates))))
    (setq ivy--old-text ivy-text)))

(defun counsel--async-command (cmd &optional process-sentinel process-filter)
  (let* ((counsel--process " *counsel*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel--process
                counsel--process
                cmd))
    (setq ivy--full-length 0)
    (setq counsel--async-start
          (setq counsel--async-time (current-time)))
    (with-current-buffer (process-buffer proc)
      (setq counsel--async-last-current-point (point-min)))
    (set-process-sentinel proc (or process-sentinel #'counsel--async-sentinel))
    (run-at-time "1 sec" nil #'counsel--async-filter)))

(defun counsel--async-filter ()
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
0.5 seconds since the last update."
  (with-current-buffer "mugu"
    (display-buffer "mugu")
    (erase-buffer)
    (call-process-shell-command (concat "grep "  "dir" " /media/sf_D_DRIVE/unsorted/counsel.temp | sort") nil "mugu")))


(defun counsel--async-sentinel (process event)
  nil
  ;; (let ((cands
  ;;        (cond ((string= event "finished\n")
  ;;               (with-current-buffer (process-buffer process)
  ;;                 (split-string
  ;;                  (buffer-string)
  ;;                  counsel-async-split-string-re
  ;;                  t)))
  ;;              ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
  ;;               (let* ((exit-code-plist (plist-get counsel--async-exit-code-plist
  ;;                                                  (ivy-state-caller ivy-last)))
  ;;                      (exit-num (read (match-string 1 event)))
  ;;                      (exit-code (plist-get exit-code-plist exit-num)))
  ;;                 (list
  ;;                  (or exit-code
  ;;                      (format "error code %d" exit-num))))))))
  ;;   (cond ((string= event "finished\n")
  ;;          (ivy--set-candidates
  ;;           (ivy--sort-maybe
  ;;            cands))
  ;;          (setq counsel-grep-last-line nil)
  ;;          (when counsel--async-start
  ;;            (setq counsel--async-duration
  ;;                  (time-to-seconds (time-since counsel--async-start))))
  ;;          (let ((re (funcall ivy--regex-function ivy-text)))
  ;;            (unless (stringp re)
  ;;              (setq re (caar re)))
  ;;            (if (null ivy--old-cands)
  ;;                (unless (setq ivy--index (ivy--preselect-index
  ;;                                          (ivy-state-preselect ivy-last)
  ;;                                          ivy--all-candidates))
  ;;                  (ivy--recompute-index
  ;;                   ivy-text re ivy--all-candidates))
  ;;              (ivy--recompute-index
  ;;               ivy-text re ivy--all-candidates)))
  ;;          (setq ivy--old-cands ivy--all-candidates)
  ;;          (if (null ivy--all-candidates)
  ;;              (ivy--insert-minibuffer "")
  ;;            (ivy--exhibit)))
  ;;         ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
  ;;          (setq ivy--all-candidates cands)
  ;;          (setq ivy--old-cands ivy--all-candidates)
  ;;          (ivy--exhibit))))
  )


;; retrieve from filter

(provide 'mugu-counsel)
