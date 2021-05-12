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
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 3)
  (company-require-match nil)
  (company-backends '((company-capf :with company-yasnippet)
                      company-files
                      (company-dabbrev-code :with company-yasnippet)
                      (company-dabbrev company-ispell :with company-yasnippet)))

  (setq company-frontends nil)
  :config
  (global-company-mode 1)
  (setq company-frontends (delete 'company-echo-metadata-frontend (custom-reevaluate-setting  'company-frontends)))
  (general-define-key
   :keymaps 'company-active-map
   "RET" #'company-complete-selection
   "M-j" #'company-select-next
   "M-k" #'company-select-previous
   [tab] #'company-complete-common-or-cycle)
   :general
   (:states '(insert)
            [remap completion-at-point] #'company-complete))

(use-package company
  :after (lispyville evil-lispy lispy)
  :general
  (:keymaps '(lispyville-mode-map lispy-mode-map evil-lispy-mode-map) :states 'insert
            "M-k" nil))

(use-package company-flx
  :diminish
  :after company
  :custom
  (company-flx-limit 250)
  :config
  (company-flx-mode 1))

(use-package company-quickhelp
  :diminish
  :disabled
  :after company
  :custom
  (company-quickhelp-delay 0)
  (company-tooltip-minimum 1)
  :config
  (company-quickhelp-mode 1))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :custom (company-box-doc-delay 0.1))

(use-package yasnippet
  :defer
  :after company
  :delight yas-minor-mode
  :general
  (:keymaps '(prog-mode-map org-mode-map)
            "²" #'yas-insert-snippet)
  :custom
  (yas-snippet-dirs (list (f-join (expand-file-name user-emacs-directory) "snippets")))
  (yas-inhibit-overlay-modification-protection t)
  (yas-wrap-around-region t)
  :config
  (yas-global-mode))
(use-package yasnippet-snippets
  :after yasnippet)

(use-package mugu-yasnippet
  :straight nil
  :after yasnippet
  :general
  (:keymaps '(prog-mode-map org-mode-map)
            "²" #'mugu-yasnippet-insert
            (general-chord "²²") #'mugu-yasnippet-menu))

(use-package yasnippet
  :defer
  :after 'ivy
  :config

  )

(provide 'mugu-conf-completion)
;;; mugu-conf-completion ends here
