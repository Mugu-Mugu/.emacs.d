;;; mugu-wgrep --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'wgrep)
(require 'evil)

(defun mugu-wgrep--edit-mode ()
  "Cleanup after mc."
  (hydra-keyboard-quit)
  (wgrep-change-to-wgrep-mode)
  (mugu-wgrep-active-menu)
  (remove-hook 'evil-normal-state-exit-hook #'mugu-wgrep--edit-mode))

(defun mugu-wgrep--prepare-menu ()
  "Install auto leave hook for multi cursor menu."
  (add-hook 'evil-normal-state-exit-hook #'mugu-wgrep--edit-mode))

(defmenu mugu-wgrep-passive-menu (:hint nil :color pink :pre (mugu-wgrep--prepare-menu))
  "Modifying the occur buffer will trigger wgrep. "
  ("q" (bury-buffer) "quit" :color blue))

(defmenu mugu-wgrep-active-menu (:hint nil :color pink)
  ("M-q" wgrep-exit "quit" :color blue)
  ("M-f" wgrep-finish-edit "finish edit" :color blue)
  ("M-d" wgrep-mark-deletion "delete line")
  ("M-u" wgrep-remove-all-change "undo everything"))

(defun mugu-wgrep-menu-delayed ()
  "Display the menu after a short delay.
Break occur otherwise..."
  (run-with-timer 0.3 nil 'mugu-wgrep-passive-menu))

(defun mugu-wgrep-activate-conf ()
  "Set my config for occur mode."
  (evil-set-initial-state 'occur 'normal)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (add-hook 'ivy-occur-grep-mode-hook 'mugu-wgrep-menu-delayed)
  (mugu-menu-register-mode-menu 'ivy-occur-grep-mode 'mugu-wgrep-passive-menu))


(provide 'mugu-wgrep)
;;; mugu-wgrep ends here
