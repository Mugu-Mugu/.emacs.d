;;; mugu-cosmetics --- Summary
;; tbc
;;; Commentary:
(require 'mugu-compat)
;;; Code:

;; * begin:
(defun mugu-cosmetics-frame-params ()
  "Return the frame params I like."
  `((horizontal-scroll-bars . nil)
    (vertical-scroll-bars . nil)
    (tool-bar-lines . nil)
    (menu-bar-lines . nil)
    (font . "Fira Code-9")
    ;; (alpha . (98 . 98))
    ,(when (mugu-compat-wsl-p) '(inhibit-double-buffering . t))))

(defun mugu-cosmetics-activate ()
  "Activate various cosmetics feature.")

(provide 'mugu-cosmetics)
;;; mugu-cosmetics ends here
