;;; mugu-conf-completion --- Summary
;; tbc
;;; Commentary:
(require 'use-package)
(require 'general)

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.25)
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
  :custom
  (company-flx-limit 250)
  :config
  (company-flx-mode 1))

(use-package company-quickhelp
  :diminish
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.25))

(use-package yasnippet
  :disabled
  :after company
  :delight yas-minor-mode
  :config
  (yas-global-mode)
  (general-def "M-&" #'yas-expand)
  (general-def "M-é" #'yas-insert-snippet))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'mugu-conf-completion)
;;; mugu-conf-completion ends here
