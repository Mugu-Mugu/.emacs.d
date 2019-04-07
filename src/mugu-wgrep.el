;;; mugu-wgrep --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'wgrep)
(require 'evil)

(defvar mugu-wgrep-timer nil "Timer for automatic menu popup.")
(defvar-local mugu-wgrep-active nil "Timer for automatic menu popup.")

(defun mugu-wgrep--edit-mode ()
  "Cleanup after mc."
  (hydra-keyboard-quit)
  (ivy-wgrep-change-to-wgrep-mode)
  (mugu-wgrep-active-menu)
  (setq mugu-wgrep-active t)
  (remove-hook 'evil-normal-state-exit-hook #'mugu-wgrep--edit-mode))

(defun mugu-wgrep--cleanup ()
  "All cleanup actions.
Cancel TIMER if still running and leave applicable window."
  (when (timerp mugu-wgrep-timer) (cancel-timer mugu-wgrep-timer))
  (when (or (eq hydra-curr-map mugu-wgrep-passive-menu-hydra/keymap)
            (eq hydra-curr-map mugu-wgrep-active-menu-hydra/keymap))
    (hydra-keyboard-quit)))

(defun mugu-wgrep--prepare-menu ()
  "Install auto leave hook for multi cursor menu."
  (add-hook 'evil-normal-state-exit-hook #'mugu-wgrep--edit-mode))

(defun mugu-wgrep-menu-maybe (buffer)
  "Ensure a wgrep menu is active in the BUFFER.
Will cancel `mugu-wgrep-timer' when buffer is killed."
  (cond ((not (eq (current-buffer) buffer)) (mugu-wgrep--cleanup))
        ((and (not hydra-curr-map) mugu-wgrep-active) (mugu-wgrep-active-menu))
        ((and (not hydra-curr-map) (equal evil-state 'normal)) (mugu-wgrep-passive-menu))
        ((not hydra-curr-map) (mugu-wgrep-active-menu))))

(defun mugu-wgrep-menu-delayed ()
  "Display the menu after a short delay.
Break occur otherwise..."
  (interactive)
  (setq mugu-wgrep-timer (run-with-idle-timer 0.1 1.0 (apply-partially #'mugu-wgrep-menu-maybe (current-buffer)))))

(defun mugu-wgrep-leave-active (with-fun)
  "Exit active mode WITH-FUN."
  (setq mugu-wgrep-active nil)
  (call-interactively with-fun)
  (mugu-wgrep-passive-menu))

(defmenu mugu-wgrep-passive-menu (:hint nil :color pink :pre (mugu-wgrep--prepare-menu))
  "Modifying the occur buffer will trigger wgrep. "
  ("q" (bury-buffer) "quit" :color blue))

(defmenu mugu-wgrep-active-menu (:hint nil :color pink)
  ("M-f" (mugu-wgrep-leave-active #'wgrep-finish-edit) "finish edit" :color blue)
  ("M-d" wgrep-mark-deletion "delete line")
  ("M-u" (mugu-wgrep-leave-active #'wgrep-abort-changes) "undo everything" :color blue))

(defun mugu-wgrep-activate-conf ()
  "Set my config for occur mode."
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (mugu-menu-register-mode-menu 'ivy-occur-grep-mode #'mugu-wgrep-menu-delayed)
  (add-hook 'ivy-occur-grep-mode-hook 'mugu-wgrep-menu-delayed))

(provide 'mugu-wgrep)
;;; mugu-wgrep ends here
