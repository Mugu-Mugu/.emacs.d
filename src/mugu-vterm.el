;;; mugu-vterm --- Summary
;; Integration with libvterm and interface utilities
;;; Commentary:
;; Enable vterm to work with evil mode
;; Also enrich missing features

;;; Code:
(require 'vterm)
(require 'ivy)
(require 'general)

(defvar mugu-vterm-list-buffer-function #'mugu-vterm-list-buffer-by-mru
  "The function to use whenever a list of vterm buffer needs to be made.")
(defvar mugu-vterm-after-vterm-creation-hook nil
  "Hook run just after a vterm is created.")

(defun mugu-vterm--install-keymaps ()
  "Configure all keymaps related to vterm integration."
  (setq vterm-keymap-exceptions nil)
  (general-define-key :keymaps 'vterm-mode-map :states 'insert
                      "<C-backspace>" (lambda () (interactive) (vterm-send-key (kbd "C-w")))
                      "<C-left>" (lambda () (interactive) (vterm-send-key "b" nil 'meta))
                      "<C-right>" (lambda () (interactive) (vterm-send-key "f" nil 'meta))
                      "C-e" #'vterm--self-insert
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
                      [remap evil-paste-before] #'vterm-yank
                      [remap undo] #'vterm-undo
                      [remap redo] #'ignore
                      "C-c" #'vterm--self-insert
                      [up] #'vterm-send-up
                      [down] #'vterm-send-down)
  ;; hard
  ;; (general-define-key :keymaps '(vterm-mode-map) :states 'normal
  ;;                     "C-r" #'vterm--self-insert)
  (general-define-key :states '(motion normal)
                      "&" #'mugu-vterm-toggle))

(defun mugu-vterm-buffer-vterm-p (buffer &rest _args)
  "Predicate indicating if BUFFER is a vterm."
  (eq 'vterm-mode (buffer-local-value 'major-mode (get-buffer buffer))))

(defun mugu-vterm-list-buffer-by-mru ()
  "Return a list of opened vterm buffer in mru order."
  (-filter 'mugu-vterm-buffer-vterm-p (buffer-list)))

(defun mugu-vterm-list-buffer ()
  "Return a list of opened vterm."
  (funcall mugu-vterm-list-buffer-function))

(defun mugu-vterm-rename (vterm-buffer new-name)
  "Rename VTERM-BUFFER to NEW-NAME."
  (interactive (list (mugu-vterm--select-terminal) (read-string "New vterm name: ")))
  (with-current-buffer vterm-buffer
    (rename-buffer (format "Vterm - %s" new-name) 'unique)))

(defun mugu-vterm--select-terminal (&optional select-other)
  "Interractively select a terminal and return it's name.
When SELECT-OTHER is non-nil, the preselected terminal is the next one."
  (ivy-read (format "Select a terminal: ")
            (-map #'buffer-name (mugu-vterm-list-buffer))
            :preselect (if select-other 1 0)
            :caller 'vterm))

(defun mugu-vterm-kill (term-name)
  "Kill vterm TERM-NAME or select one to kill."
  (interactive (list (mugu-vterm--select-terminal)))
  (let ((kill-buffer-query-functions nil))
    (save-current-buffer (kill-buffer term-name))))

(defun mugu-vterm-switch (&optional select-first select-other)
  "Switch to a vterm buffer interactively if there is several open.
If none exists, one will be created.
If SELECT-FIRST is non-nil, select the first buffer in the list
`mugu-vterm-list-buffer'.  If SELECT-OTHER is non-nil and SELECT-FIRST is nil,
select the second buffer in the list instead."
  (interactive)
  (let* ((base-vterm-list (mugu-vterm-list-buffer))
         (vterm-list (if select-first
                         (-take 1 base-vterm-list)
                       base-vterm-list)))
    (pcase (length vterm-list)
      (0 (mugu-vterm-create))
      (1 (switch-to-buffer (-first-item vterm-list)))
      (_ (switch-to-buffer (get-buffer
                            (mugu-vterm--select-terminal select-other)))))))

(defun mugu-vterm-toggle ()
  "Switch to a vterm buffer or hide one if already displayed."
  (interactive)
  (let ((window-displaying-vterm (get-window-with-predicate
                                  (lambda (window) (mugu-vterm-buffer-vterm-p (window-buffer window))))))
    (if window-displaying-vterm
        (mugu-vterm-switch nil 'select-other)
      (mugu-vterm-switch 'select-first))))

(defun mugu-vterm-create (&optional name)
  "Create a new vterm with NAME if given."
  (interactive (list (read-string "Vterm name? ")))
  (switch-to-buffer
   (save-window-excursion
     (vterm (if name
                (format "vterm (%s)" name)
              "vterm"))
     (run-hooks 'mugu-vterm-after-vterm-creation-hook)
     (current-buffer))))

(defun vterm-evil-insert ()
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-insert))

(defun vterm-evil-append ()
  (interactive)
  (vterm-goto-char (1+ (point)))
  (call-interactively #'evil-append))

(defun vterm-evil-delete ()
  "Provide similar behavior as `evil-delete'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-delete))))

(defun vterm-evil-change ()
  "Provide similar behavior as `evil-change'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-change))))

;; (defun my-vterm-hook()
;;   (evil-local-mode 1)
;;   (evil-define-key 'normal 'local "a" 'vterm-evil-append)
;;   (evil-define-key 'normal 'local "d" 'vterm-evil-delete)
;;   (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
;;   (evil-define-key 'normal 'local "c" 'vterm-evil-change))

(defun mugu-vterm-activate ()
  "Configure vterm integration."
  (mugu-vterm--install-keymaps)
  ;; (add-hook 'vterm-mode-hook 'my-vterm-hook)
  (ivy-add-actions 'vterm
                   '(("r" mugu-vterm-rename "rename")
                     ("k" mugu-vterm-kill "kill")))
  (mugu-window-configure-side-window 'mugu-vterm-buffer-vterm-p 'top 0.7))

(provide 'mugu-vterm)
;;; mugu-vterm ends here
