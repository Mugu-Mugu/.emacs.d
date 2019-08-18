;;; mugu-vterm --- Summary
;; Integration with libvterm and my configuration
;;; Commentary:
;; Enable vterm to work with evil mode
;; Also enrich missing features

;;; Code:
(require 'vterm)
(require 'ivy)
(require 'general)
(require 'mugu-feature)

(defvar mugu-vterm--cursor-pos 0
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
  (general-define-key :keymaps '(vterm-mode-map)
                      [remap evil-paste-after] #'mugu-vterm-paste)
  (general-define-key :keymaps '(vterm-mode-map) :states '(insert)
                      "C-r" #'mugu-vterm-send-reverse-search
                      "C-w" #'mugu-vterm-send-kill-arg
                      "C-y" #'mugu-vterm-send-yank))

(defun mugu-vterm-buffer-vterm-p (buffer)
  "Predicate indicating if BUFFER is a vterm."
  (eq 'vterm-mode (buffer-local-value 'major-mode buffer)))

(defun mugu-vterm-paste ()
  "Paste current kill content at current cursor position."
  (interactive)
  (mugu-vterm--move-real-cursor)
  (vterm-yank)
  (mugu-vterm--record-cursor-pos (length (substring-no-properties (current-kill 0)))))

(defun mugu-vterm-send-kill-arg ()
  "Send ctrl-w."
  (interactive)
  (vterm-send-key "w" nil nil 'ctrl))

(defun mugu-vterm-send-yank ()
  "Send ctrl-y."
  (interactive)
  (vterm-send-key "y" nil nil 'ctrl))

(defun mugu-vterm-send-reverse-search ()
  "Send ctrl-r."
  (interactive)
  (vterm-send-key "r" nil nil 'ctrl))

(defun mugu-vterm-switch ()
  "Switch to a vterm buffer.
If none exists, one will be created."
  (interactive)
   (let* ((existing-vterms (-filter #'mugu-vterm-buffer-vterm-p (buffer-list))))
    (pcase (length existing-vterms)
      (0 (vterm))
      (1 (mugu-feature-switch-buffer (-first-item existing-vterms)))
      (_ (mugu-feature-switch-buffer (get-buffer
                                      (ivy-read (format "Select a terminal: " )
                                                (-map #'buffer-name existing-vterms))))))))

(defun mugu-vterm-activate ()
  "Configure vterm integration."
  (mugu-vterm--install-keymaps)
  (add-hook 'vterm-mode-hook #'mugu-vterm--install-hook))

(provide 'mugu-vterm)
;;; mugu-vterm ends here
