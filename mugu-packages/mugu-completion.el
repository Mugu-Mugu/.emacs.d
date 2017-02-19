;;; mugu-completion --- Summary
;; tbc
;;; Commentary:

;;; Code:
(use-package company
  :ensure
  :diminish company-mode
  :defer 3
  :bind
  (:map company-active-map
        ("M-j" . company-select-next)
        ("M-k" . company-select-previous)
        ("M-j" . company-select-next)
        ("M-k" . company-select-previous)
        ("<tab>" . company-complete-selection)
        ("<return>" . company-return-exit)
        ("SPC" . mugu-company-space-exit))
  :functions company-abort
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '((company-capf)))))
  (defun mugu-company-return-exit ()
    (company-abort))
  (defun mugu-company-space-exit ()
    (company-abort)
    (insert " ")))

(use-package company-flx
  :ensure
  :diminish
  :after company
  :config (company-flx-mode 1))

(use-package company-quickhelp
  :ensure
  :diminish
  :after company
  :config (progn
            (company-quickhelp-mode 1)
            (setq company-quickhelp-delay 0.3)))

(provide 'mugu-completion)
;;; mugu-completion ends here
