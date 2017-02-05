(require 'shell)
(require 'mugu-core)
(require 'mugu-directory-fix)

;; on insert mode, autoscroll to end of buffer
(defun mugu-shell-scroll-before-insert ()
  "scroll to prompt if needed"
  (interactive)
  (unless (equal (line-number-at-pos (point-max)) (line-number-at-pos))
    (comint-show-maximum-output)))

(after 'counsel
  (defun mugu-shell--find-file-recursive (&optional initial-input initial-directory)
    "Jump to a file from a list of all files directories
below the current one.  INITIAL-INPUT can be given as the initial
minibuffer input.  INITIAL-DIRECTORY, if non-nil, is used as the
root directory for search."
    (interactive
     (mugu-shell-scroll-before-insert)
     (list nil
           (when current-prefix-arg
             (read-directory-name "From directory: "))))
    (let* ((default-directory (or initial-directory default-directory)))
      (ivy-read "Find file: "
                (split-string
                 (shell-command-to-string "find * -type f -not -path '*\/.git*'")
                 "\n" t)
                :matcher #'counsel--find-file-matcher
                :initial-input initial-input
                :action (lambda (x)
                          (with-ivy-window
                            (insert (expand-file-name x ivy--directory))))
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'counsel-file-jump)))

  (defun mugu-shell--find-dir-recursive (&optional initial-input initial-directory)
    "Interactly select a directory recursivly and insert it in current buffer.
INITIAL-INPUT and INITIAL-DIRECTORY"
    (interactive
     (list nil
           (when current-prefix-arg
             (read-directory-name "From directory: "))))
    (let* ((default-directory (or initial-directory default-directory)))
      (ivy-read "Directory: "
                (split-string
                 (shell-command-to-string "find * -type d -not -path '*\/.git*'")
                 "\n" t)
                :initial-input initial-input
                :history 'file-name-history
                :action (lambda (d)
                          (with-ivy-window
                            (insert (concat "cd " (expand-file-name d)))))
                :caller 'counsel-dired-jump)))

(defun mugu-shell--load-bookmark-dir ()
  (interactive)
  (require 'cl-lib)
  (ivy-read "Select bookmark name"
            (cl-remove-if-not
             (lambda (x) (file-directory-p (bookmark-location x)))
             (bookmark-all-names))
            :action (lambda (x)
                      (with-ivy-window 
                        (insert (concat "cd " (expand-file-name (bookmark-location x))))))
            :caller 'counsel-bookmark))

  (defun mugu-shell--find-dir ()
    (interactive)
    (insert (concat "cd "
                    (expand-file-name (read-directory-name "select directory ")))))

  (defun mugu-shell--find-file ()
    (interactive)
    (let ((file-or-dir-result (expand-file-name (read-file-name "select file"))))
      (if (directory-name-p file-or-dir-result)
          (mugu-shell-change-directory (lambda ()
                                         (interactive)
                                         (insert (concat "cd " file-or-dir-result))))
        (insert file-or-dir-result)))))



(defun mugu-shell-change-directory (select-dir-fun)
  "Wrapper to ensure the directory change will be safe.
directory is selected through SELECT-DIR-FUN"
  (interactive)
  (comint-show-maximum-output)
  (comint-kill-input)
  (call-interactively select-dir-fun)
  (comint-send-input)
  (mugu-directory-with-current-file-path))

(defun mugu-shell-send-input ()
  "Ensure the command sent is safe (standard one may be dangerous because of evil)."
  (interactive)
  (comint-show-maximum-output)
  (comint-send-input))


(after 'evil
  ;;  modify change line behaviour to apply only on the prompt regardless of point location
  (defun mugu-shell-change-line ()
    (interactive)
    (mugu-shell-scroll-before-insert)
    (comint-bol)
    (call-interactively 'evil-change-line))
  (defun mugu-shell-change ()
    (interactive)
    (mugu-shell-scroll-before-insert)
    (call-interactively 'evil-change)))

(provide 'mugu-shell-utils) 
