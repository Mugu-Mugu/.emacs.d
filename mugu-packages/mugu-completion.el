;;; mugu-completion --- Summary
;; tbc
;;; Commentary:
(require 'use-package)

;;; Code:
(use-package company
  :diminish company-mode
  :defer 3
  :bind
  (:map company-active-map
        ("M-j" . company-select-next)
        ("M-k" . company-select-previous)
        ("M-i" . company-select-previous)
        ("<tab>" . company-complete-selection)
        ("<return>" . company-return-exit)
        ("SPC" . mugu-company-space-exit))
  :functions company-abort
  :config
  (global-company-mode +1)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '((company-capf)))))
  (defun mugu-company-return-exit ()
    (interactive)
    (company-abort))
  (defun mugu-company-space-exit ()
    (interactive)
    (company-abort)
    (insert " ")))

(use-package company-flx
  :diminish
  :after company
  :config (company-flx-mode 1))

(use-package company-quickhelp
  :diminish
  :after company
  :config (progn
            (company-quickhelp-mode 1)
            (setq company-quickhelp-delay 0.3)))

(provide 'mugu-completion)
;;; mugu-completion ends here
