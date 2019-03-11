;;; mugu-conf-completion --- Summary
;; tbc
;;; Commentary:
(require 'use-package)
(require 'general)

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)
  (company-require-match nil)
  :config
  (general-unbind lispy-mode-map "M-k")
  (general-define-key :keymaps 'company-active-map
                      "M-k" #'company-select-previous
                      "M-j" #'company-select-next
                      "<tab>" #'company-complete-selection)
  (global-company-mode +1))

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

(provide 'mugu-conf-completion)
;;; mugu-conf-completion ends here
