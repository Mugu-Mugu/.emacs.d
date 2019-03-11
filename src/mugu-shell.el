;;; mugu-shell --- Summary
;; tbc
;;; Commentary:

;;; Code:

;;; Code:
(require 'shell)
(require 'mugu-misc)
(require 'mugu-directory)
(require 'mugu-menu)
(require 'general)

;; on insert mode, autoscroll to end of buffer
(defun mugu-shell-scroll-before-insert ()
  "Scroll to prompt if needed."
  (interactive)
  (unless (equal (line-number-at-pos (point-max)) (line-number-at-pos))
    (comint-show-maximum-output)))

(defun mugu-shell-change-directory (select-dir-fun &rest args)
  "Wrapper to ensure the directory change will be safe.
directory is selected through SELECT-DIR-FUN on which is applied ARGS"
  (interactive)
  (comint-show-maximum-output)
  (comint-kill-input)
  (insert (concat "cd " (apply select-dir-fun args)))
  (mugu-directory-cd default-directory)
  (comint-send-input)
  (mugu-directory-with-current-file-path))

(defun mugu-shell-send-input ()
  "Ensure the command sent is safe (standard one may be dangerous because of evil)."
  (interactive)
  (comint-show-maximum-output)
  (comint-send-input))

;;  modify change line behaviour to apply only on the prompt regardless of point location
(defun mugu-shell-change-line ()
  "."
  (interactive)
  (mugu-shell-scroll-before-insert)
  (comint-bol)
  (call-interactively 'evil-change-line))

(defun mugu-shell-change ()
  "."
  (interactive)
  (mugu-shell-scroll-before-insert)
  (call-interactively 'evil-change))

(defmenu mugu-shell-menu
  (:color blue :hint nil :body-pre (lambda ()
                                     (shell-resync-dirs)
                                     (mugu-shell-scroll-before-insert)))
  "
                                -- SHELL MENU --
  -> Current Dir : %s(mugu-directory-pwd-file)
  -> Mugu Dir : %s(mugu-directory-pwd)
"
  ("d" (mugu-shell-change-directory (mugu-directory-pwd)) "find dir" :column "1-find")
  ("cd" (mugu-shell-change-directory 'read-directory-name "directory:") "find dir" :column "1-find")
  ("cr" (mugu-shell-change-directory 'mugu-counsel-find-dir-recursive) "find dir recursively")
  ("cm" (mugu-shell-change-directory 'mugu-counsel-read-bookmark-dir) "find dir from bookmark")
  ("md" (mugu-shell-change-directory 'mugu-counsel-read-bookmark-dir) "find dir from bookmark")
  ("ff" (insert (read-file-name "select file:  ")) "find file")
  ("fr" (insert (mugu-counsel-find-file-recursive)) "find file recursively")
  ("t" comint-truncate-buffer "trucate shell" :column "2-command")
  ("a" counsel-shell-history "again command")
  ("C-r" counsel-shell-history "again command"))

(defun mugu-shell-legacy-bindings ()
  "Aret they still usefull?"
  (general-def shell-mode-map
    "C-x" #'comint-get-next-from-history
    "C-a" #'comint-bol-or-process-mark
    "C-u" #'comint-kill-input
    "C-w" #'backward-kill-word
    "C-y" #'yank
    "C-c" #'comint-interrupt-subjob
    "C-z" #'comint-stop-subjob
    "C-\\" #'comint-quit-subjob
    "RET" #'mugu-shell-send-input
    "C-o" #'comint-delete-output
    "M-o" #'comint-clear-buffer
    "C-e" #'comint-show-maximum-output
    "C-l" #'comint-dynamic-list-input-ring
    "C-n" #'comint-next-prompt
    "C-p" #'comint-previous-prompt
    "C-d" #'comint-send-eof
    "C-s" #'comint-write-output
    [up] #'comint-previous-input
    [down] #'comint-next-input
    "C-r" #'comint-history-isearch-backward-regexp
    "M-s" #'comint-next-matching-input-from-input))

(defun mugu-shell-activate-evil-conf ()
  "."
  (add-hook 'shell-mode-hook 'superword-mode)

  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook 'evil-insert-state-entry-hook 'mugu-shell-scroll-before-insert nil 'make-it-local)))

  (evil-define-key 'normal shell-mode-map "c" 'mugu-shell-change)
  (evil-define-key 'normal shell-mode-map "C" 'mugu-shell-change-line)

  ;; in shell o and O have no point
  (evil-define-key 'normal shell-mode-map "o" 'evil-append-line)
  (evil-define-key 'normal shell-mode-map "o" 'evil-append-line))

(provide 'mugu-shell)
;;; mugu-shell ends here
