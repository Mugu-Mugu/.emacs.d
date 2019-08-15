;;; mugu-cosmetics --- Summary
;; tbc
;;; Commentary:
(require 'mugu-compat)
;;; Code:

;; * begin:
(defun mugu-cosmetics-frame-params ()
  "Return the frame params I like."
  `((fullscreen . maximized)
    (horizontal-scroll-bars . nil)
    (vertical-scroll-bars . nil)
    (tool-bar-lines . nil)
    (menu-bar-lines . nil)
    (font . "DejaVu Sans Mono-9:bold")
    (alpha . (95 . 95))
    ,(when (mugu-compat-wsl-p) '(inhibit-double-buffering . t))))

(defun mugu-cosmetics-activate ()
  "Activate various cosmetics feature.")

(provide 'mugu-cosmetics)
;;; mugu-cosmetics ends here
