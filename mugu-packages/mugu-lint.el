(require 'hydra)


(use-package flycheck
  :diminish flycheck-mode
  :defer
  :commands flycheck-list-errors
  :init
  (defun mugu-lint-previous-error ()
    "Really go to previous error."
    (let ((point-before (point))
          (increment 1))
      (flycheck-previous-error)
      (while (equal (point) point-before)
        (flycheck-previous-error (incf increment)))))

  (defhydra mugu-lint-menu
               (:color pink :hint nil :idle 0.1
                       :body-pre (flycheck-list-errors)
                       :post (delete-window (flycheck-get-error-list-window)))
               "
                              -- Flycheck Zone --
"
               ("j" flycheck-next-error "↓ error" :column "1-Errors")
               ("k" (mugu-lint-previous-error) "↑ error")
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
  :config
  (global-flycheck-mode 1)

  (customize-set-value 'flycheck-emacs-lisp-load-path 'inherit)
  (customize-set-value 'flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (customize-set-value 'flycheck-idle-change-delay 5)
  ;; (setq-default flycheck-disabled-checkers (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers))

  (add-to-list 'display-buffer-alist
               (quote ("\\*Flycheck errors\\*" . ((display-buffer-in-side-window)
                                                  .
                                                  ((side . bottom)
                                                   (window-height . 10)
                                                   (window-width . 1)
                                                   (inhibit-switch-frame . t)
                                                   (inhibit-same-window . t)))))))

(use-package flycheck-pos-tip
  ;; its nice but it cause jerky refresh
  ;; it will be reactivated once eamcs 25.2 is released with the double buffer
  ;; :disabled
  :disabled
  :after flycheck
  :config (flycheck-pos-tip-mode))

(provide 'mugu-lint)
;;; mugu-lint ends here
