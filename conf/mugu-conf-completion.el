;;; mugu-conf-completion --- Summary
;; tbc
;;; Commentary:
(require 'use-package)
(require 'general)
(require 'f)

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-require-match nil)
  (company-backends '(company-capf (company-dabbrev-code) company-dabbrev))
  :general
  (:keymaps '(company-active-map)
            "M-j" #'company-select-next
            "M-k" #'company-select-previous
            "RET" #'company-complete-selection
            [tab] #'company-complete-common-or-cycle)
  :config
  (global-company-mode 1))

(use-package company
  :after lispy
  :general
  (:keymaps '(evil-lispy-mode-map lispyville-mode-map lispy-mode-map)
            :states 'insert "M-k" nil))

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
  :custom
  (company-quickhelp-delay 0)
  (company-tooltip-minimum 1)
  :config
  (company-quickhelp-mode 1))

(use-package yasnippet
  :defer
  :after company
  :delight yas-minor-mode
  :general
  (:keymaps '(prog-mode-map org-mode-map)
            "²" #'yas-insert-snippet)
  :custom
  (yas-snippet-dirs (list (f-join (expand-file-name user-emacs-directory) "snippets")))
  :config
  (yas-global-mode)
  (push '(company-capf :with company-yasnippet) company-backends))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package mugu-yasnippet
  :straight nil
  :after yasnippet
  :general
  (:keymaps '(prog-mode-map org-mode-map)
            "²" #'mugu-yasnippet-insert
            (general-chord "²²") #'mugu-yasnippet-menu))

(provide 'mugu-conf-completion)
;;; mugu-conf-completion ends here
