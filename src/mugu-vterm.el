;;; mugu-vterm --- Summary
;; Integration with libvterm and my configuration
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
                      "C-c" #'vterm--self-insert))

(defun mugu-vterm-buffer-vterm-p (buffer &rest _args)
  "Predicate indicating if BUFFER is a vterm."
  (eq 'vterm-mode (buffer-local-value 'major-mode (get-buffer buffer))))

(defun mugu-vterm-paste ()
  "Paste current kill content at current cursor position."
  (interactive)
  (mugu-vterm--move-real-cursor)
  (vterm-yank)
  (mugu-vterm--record-cursor-pos (length (substring-no-properties (current-kill 0)))))

 (defun mugu-vterm-switch ()
  "Switch to a vterm buffer.
If none exists, one will be created."
  (interactive)
   (let* ((existing-vterms (-filter #'mugu-vterm-buffer-vterm-p (buffer-list))))
     (pcase (length existing-vterms)
       (0 (mugu-vterm-create))
       (1 (mugu-buffer-switch (-first-item existing-vterms)))
       (_ (mugu-buffer-switch (get-buffer
                               (ivy-read (format "Select a terminal: ")
                                         (-map #'buffer-name existing-vterms))))))))

(defun mugu-vterm-create (&optional name)
  "Create a new vterm with NAME if given."
  (mugu-buffer-switch
   (save-window-excursion
     (vterm (format "vterm (%s)" name)))))

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
                 (inhibit-same-window . nil))))

(provide 'mugu-vterm)
;;; mugu-vterm ends here
