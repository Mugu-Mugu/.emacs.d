;;; mugu-vterm --- Summary
;; Integration with libvterm and interface utilities
;;; Commentary:
;; Enable vterm to work with evil mode
;; Also enrich missing features

;;; Code:
(require 'vterm)
(require 'ivy)
(require 'general)
(require 'mugu-buffer)

(defvar-local mugu-vterm--cursor-pos 0
  "Pos of vterm cursor.")
(defvar mugu-vterm-list-buffer-function #'mugu-vterm-list-buffer-by-mru
  "The function to use whenever a list of vterm buffer needs to be made.")
(defvar mugu-vterm-after-vterm-creation-hook nil
  "Hook run just after a vterm is created.")

(defun mugu-vterm--record-cursor-pos (&optional offset)
  "Record the current position of the vterm cursor.
If OFFSET is non nil, add it to the current `point'.
It's up to the caller to ensure current `point' and the real vterm cursor are
synched"
  (setq mugu-vterm--cursor-pos (+ (point) (or offset 0))))

(defun mugu-vterm--move-real-cursor ()
  "Send command to vterm to synch its cursor to the current one."
  (let* ((cursor-difference (- (point) mugu-vterm--cursor-pos))
         (movement-function (if (> cursor-difference 0) #'vterm-send-right #'vterm-send-left)))
    (dotimes (_ (abs cursor-difference))
      (funcall movement-function))
    (mugu-vterm--record-cursor-pos)))

(defun mugu-vterm--install-hook ()
  "Install all hook required for vterm."
  (interactive)
  (add-hook 'evil-insert-state-exit-hook #'mugu-vterm--record-cursor-pos nil 'local)
  (add-hook 'evil-insert-state-entry-hook #'mugu-vterm--move-real-cursor nil 'local))

(defun mugu-vterm--install-keymaps ()
  "Configure all keymaps related to vterm integration."
  (setq vterm-keymap-exceptions nil)
  (general-define-key :keymaps 'vterm-mode-map :states 'insert
                      "C-e" #'vterm--self-insert
                      "C-f" #'vterm--self-insert
                      "C-a" #'vterm--self-insert
                      "C-v" #'vterm--self-insert
                      "C-b" #'vterm--self-insert
                      "C-w" #'vterm--self-insert
                      "C-u" #'vterm--self-insert
                      "C-d" #'vterm--self-insert
                      "C-n" #'vterm--self-insert
                      "C-m" #'vterm--self-insert
                      "C-p" #'vterm--self-insert
                      "C-j" #'vterm--self-insert
                      "C-k" #'vterm--self-insert
                      "C-r" #'vterm--self-insert
                      "C-t" #'vterm--self-insert
                      "C-y" #'vterm--self-insert
                      "C-g" #'vterm--self-insert
                      "C-c" #'vterm--self-insert
                      "C-SPC" #'vterm--self-insert
                      "ESC" #'vterm--self-insert)
  (general-define-key :keymaps '(vterm-mode-map) :states 'motion
                      [remap evil-paste-after] #'mugu-vterm-paste
                      [remap undo] #'vterm-undo
                      [remap redo] #'ignore
                      "C-c" #'vterm--self-insert)
  (general-define-key :states 'motion
                      "²" #'mugu-vterm-toggle))

(defun mugu-vterm-buffer-vterm-p (buffer &rest _args)
  "Predicate indicating if BUFFER is a vterm."
  (eq 'vterm-mode (buffer-local-value 'major-mode (get-buffer buffer))))

(defun mugu-vterm-list-buffer-by-mru ()
  "Return a list of opened vterm buffer in mru order."
  (-filter 'mugu-vterm-buffer-vterm-p (buffer-list)))

(defun mugu-vterm-list-buffer ()
  "Return a list of opened vterm."
  (funcall mugu-vterm-list-buffer-function))

(defun mugu-vterm-paste ()
  "Paste current kill content at current cursor position."
  (interactive)
  (mugu-vterm--move-real-cursor)
  (vterm-yank)
  (mugu-vterm--record-cursor-pos (length (substring-no-properties (current-kill 0)))))

(defun mugu-vterm-rename (new-name)
  "Rename current vterm with NEW-NAME."
  (interactive)
  (unless (mugu-vterm-buffer-vterm-p (current-buffer))
    (error "Current buffer %s is not a vterm" (current-buffer)))
  (rename-buffer (format "Vterm - %s" new-name)))

(defun mugu-vterm-switch (&optional select-first)
  "Switch to a vterm buffer interactively if there is several open.
If none exists, one will be created.
If SELECT-FIRST is non-nil, select the first buffer in the list `mugu-vterm-list-buffer'."
  (interactive)
  (let* ((vterm-list (if select-first
                         (-take 1 (mugu-vterm-list-buffer))
                       (mugu-vterm-list-buffer))))
    (pcase (length vterm-list)
      (0 (mugu-vterm-create))
      (1 (mugu-buffer-switch (-first-item vterm-list)))
      (_ (mugu-buffer-switch (get-buffer
                              (ivy-read (format "Select a terminal: ")
                                        (-map #'buffer-name vterm-list))))))))

(defun mugu-vterm-toggle ()
  "Switch to a vterm buffer or hide one if already displayed."
  (interactive)
  (let ((window-displaying-vterm (get-window-with-predicate
                                  (lambda (window) (mugu-vterm-buffer-vterm-p (window-buffer window))))))
    (if window-displaying-vterm
        (delete-window window-displaying-vterm)
      (mugu-vterm-switch 'select-first))))

(defun mugu-vterm-create (&optional name)
  "Create a new vterm with NAME if given."
  (mugu-buffer-switch
   (save-window-excursion
     (vterm (if name
                (format "vterm (%s)" name)
              "vterm"))
     (run-hooks 'mugu-vterm-after-vterm-creation-hook)
     (current-buffer))))

(defun mugu-vterm-activate ()
  "Configure vterm integration."
  (mugu-vterm--install-keymaps)
  (add-hook 'vterm-mode-hook #'mugu-vterm--install-hook)
  (add-to-list 'display-buffer-alist
               '(mugu-vterm-buffer-vterm-p
                 (display-buffer-in-side-window)
                 (side . top)
                 (slot . 0)
                 (window-height . 0.5)
                 (inhibit-same-window . nil)))
  )

(provide 'mugu-vterm)
;;; mugu-vterm ends here
