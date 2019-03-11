;;; mugu-flycheck --- Summary
;; tbc
;;; Commentary:
(require 'mugu-menu)
(require 'flycheck)

;;; Code:
(defun mugu-flycheck-previous-error ()
  "Really go to previous error."
  (let ((point-before (point))
        (increment 1))
    (flycheck-previous-error)
    (while (equal (point) point-before)
      (flycheck-previous-error (incf increment)))))

(defmenu mugu-flycheck-menu
  (:color red :hint nil
          :body-pre (flycheck-list-errors)
          :post (delete-window (flycheck-get-error-list-window)))
  "
                              -- Flycheck Zone --
"
  ("j" flycheck-next-error "↓ error" :column "1-Errors")
  ("k" (mugu-flycheck-previous-error) "↑ error")
  ("y" flycheck-copy-errors-as-kill "Copy errors")
  ("v" flycheck-buffer "Recheck buffer" :column "2-Buffer Management")
  ("p" flycheck-clear "Ignore errors")
  ("c" flycheck-compile "Compile buffer")
  ("s" flycheck-select-checker "select checker" :column "3-Checker Management")
  ("d" flycheck-describe-checker "describe checker")
  ("x" flycheck-disable-checker "disable checker")
  ("av" flycheck-version "display version" :column nil)
  ("ac" flycheck-verify-setup "display setup")
  ("q" nil "cancel hydra" :color blue))

(defun mugu-flycheck-set-window-conf ()
  "."
  (add-to-list 'display-buffer-alist
               (quote ("\\*Flycheck errors\\*" . ((display-buffer-in-side-window)
                                                  .
                                                  ((side . bottom)
                                                   (window-height . 10)
                                                   (window-width . 1)
                                                   (inhibit-switch-frame . t)
                                                   (inhibit-same-window . t)))))))

(provide 'mugu-flycheck)
;;; mugu-flycheck ends here
