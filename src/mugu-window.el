;;; mugu-window --- Basic settings for vanilla emacs window -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'hydra)
(require 'winner)
(require 'use-package)
(require 'ace-window)

(defun lunaryorn-find-side-windows (&optional side)
  "Get all side window if any.
If SIDE is non-nil only get windows on that side."
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (let ((window-side (window-parameter window 'window-side)))
         (when (and window-side (or (not side) (eq window-side side)))
           (push window windows)))))
    windows))

(defun lunaryorn-quit-all-side-windows ()
  "Quit all side windows of the current frame."
  (interactive)
  (dolist (window (lunaryorn-find-side-windows))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

(defun mugu/delete-others-windows ()
  "Quit all others windows, including side one."
  (interactive)
  (delete-other-windows)
  (lunaryorn-quit-all-side-windows))

(defun mugu-window-delete-next-side ()
  "Delete the next side window."
  (interactive)
  (let ((next-side-window (window-with-parameter 'window-side)))
    (if next-side-window
        (delete-window next-side-window)
      (warn "There is no side window open."))))

(defmenu mugu-window-resize-menu
  (:color red :hint nil)
  "
                             -- RESIZING WINDOW --
"
  ("b" balance-windows "balance window height" :column "0-Set")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "maximize current window")
  ("h" shrink-window-horizontally "↔ shrink"  :column "1-Shrink")
  ("j" shrink-window  "↕ shrink")
  ("H" (shrink-window-horizontally 20) "↔ shrink ++")
  ("J" (shrink-window 20) "↕ shrink ++")
  ("k" "enlarge-window" "↔ enlarge" :column "2-Enlarge")
  ("l" enlarge-window-horizontally "↕ enlarge")
  ("K" (enlarge-window 20) "↔ enlarge")
  ("L" (enlarge-window-horizontally 20) "↕ enlarge")
  ("RET" mugu-window-menu "return to window menu" :color blue :column nil)
  ("q" nil "quit menu" :color blue :column nil))

(defmenu mugu-window-menu
  (:color red :hint nil :timeout 4)
  "
                               -- WINDOW MENU --

"
  ("z" ace-window "ace" :color blue :column "1-Switch")
  ("h" windmove-left "← window")
  ("j" windmove-down "↓ window")
  ("k" windmove-up "↑ window")
  ("l" windmove-right "→ window")
  ("s" split-window-below "split window" :color blue :column "2-Split Management")
  ("v" split-window-right "split window vertically" :color blue)
  ("d" delete-window "delete current window")
  ("D" mugu/delete-others-windows "delete *all* other windows")
  ("u" winner-undo "undo window conf" :column "3-Undo/Redo")
  ("r" winner-redo "redo window conf")
  ("b" balance-windows "balance window height" :column "4-Sizing")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "maximize current window")
  ("c" mugu-window-resize-menu "resize window size submenu" :color blue)
  ("q" nil "quit menu" :color blue :column nil))




(provide 'mugu-window)
;;; mugu-window ends here
