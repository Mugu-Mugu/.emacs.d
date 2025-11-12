;;; mugu-conf-completion --- Summary
;; tbc
;;; Commentary:
(require 'use-package)
(require 'general)
(require 'f)

;;; Core
(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 3)
  (company-require-match nil)
  (company-auto-commit nil)
  (company-auto-complete nil)
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
            [remap indent-for-tab-command] #'company-indent-or-complete-common
            [remap completion-at-point] #'company-complete))

(use-package company
  :after (lispyville evil-lispy lispy)
  :general
  (:keymaps '(lispyville-mode-map lispy-mode-map evil-lispy-mode-map) :states 'insert
            "M-k" nil))

;;; Completion sources
(use-package company
  :defer
  :custom
  (company-backends '(;; this should be the main backend
                      (company-capf :with company-yasnippet)
                      ;; not in the same backend as capf because if the backend offer semantic completion
                      ;; then the dumb one from dabbrev is not welcome
                      (company-dabbrev-code :with company-keywords company-yasnippet)
                      ;; it makes senses to have it alone and after other to prevent a file in current
                      ;; directory to shadow legitimate results. Besides most of the time, a completion on
                      ;; file should start by ../ or ~/ or ./ which should be exclusive to this backend
                      (company-files)
                      ;; dabbrev is after becasue it could match all sort of junk
                      (company-dabbrev :with company-yasnippet)
                      ;; last but not least snippet alone as they were previously included as an optional
                      ;; result
                      (company-yasnippet))))

(use-package company-restclient
  :after restclient
  :config
  (push '(company-restclient :with company-yasnippet) company-backends))

(use-package slack
  :defer
  :config
  (push '(company-slack-backend :with company-yasnippet company-files company-dabbrev)
        company-backends))

(use-package robe
  :after company
  :defer
  :config
  (push '(company-robe :with company-keywords company-dabbrev-code company-yasnippet) company-backends))

;;; Front end
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
  :custom (company-box-doc-delay 0.3))

;;; Snippets
(use-package yasnippet
  :after company
  :demand
  :delight yas-minor-mode
  :general
  (:keymaps '(prog-mode-map org-mode-map yaml-mode-map)
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
  (:keymaps '(prog-mode-map org-mode-map yaml-mode-map)
            "²" #'mugu-yasnippet-insert
            (general-chord "²²") #'mugu-yasnippet-menu))


;;; History
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode)
  :custom
  (company-prescient-sort-length-enable nil))

;;; Completion styles and filtering
(use-package mugu-vanilla
  ;; common completion settings
  :straight nil
  :defer
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

(use-package orderless
  :defer
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-prefixes)))

(use-package mugu-orderless
  :straight nil
  :after orderless
  :custom
  (orderless-style-dispatchers '(mugu-orderless-flex-if-twiddle
                                 mugu-orderless-first-with-initialism
                                 mugu-orderless-without-if-bang)))

(use-package orderless
  :after (company mugu-orderless)
  :demand
  :config
  (advice-add 'company-capf--candidates :around #'mugu-orderless-company-face-advice)
  :custom
  (orderless-component-separator "[ &]"))

(use-package orderless
  :after ivy
  :demand
  :config
  (push '(t . orderless-ivy-re-builder) ivy-re-builders-alist)
  (push '(counsel-rg . ivy--regex-ignore-order) ivy-re-builders-alist))

(use-package lsp-mode
  :after company yasnippet
  :custom
  (lsp-enable-snippet t))

(provide 'mugu-conf-completion)
;;; mugu-conf-completion ends here
