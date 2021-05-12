;;; mugu-ea --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun mugu-popup-handler (app-name window-title x y w h)
  "Configure popup frame for emacs-everywhere.
APP-NAME WINDOW-TITLE X Y W H."
  (set-frame-position (selected-frame) x (+ y (- h 400)))
  (set-frame-size (selected-frame) (max 600 w) 400 t)
  (evil-insert-state))

(provide 'mugu-ea)
;;; mugu-ea ends here
