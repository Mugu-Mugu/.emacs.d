;;; mugu-lsp --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)
(require 'mugu-menu)
(require 'general)

(use-package lsp-mode
  :commands lsp mugu-lsp-menu
  :custom
  (lsp-prefer-flymake nil)
  :config
  (require 'lsp-ui)
  (defmenu mugu-lsp-menu (:color blue :hint nil)
    "-- Language menu -- "
    ("g" lsp-find-definition "goto def" :column "Navigation")
    ("x" lsp-ui-peek-find-references "xref lookup")
    ("s" imenu "structure menu")
    ("f" lsp-execute-code-action "fix code" :column "Actions")
    ("F" lsp-format-buffer "fix file")
    ("r" lsp-rename "rename thing")
    ("dd" lsp-ui-doc-mode "display doc" :column "Display" :color red)
    ("ds" lsp-ui-sideline-toggle-symbols-info "display symbol" :color red)
    ("md" lsp-describe-session "describe lsp session" :column "Management")
    ("mr" lsp-restart-workspace "Restart project")
    ("ma" lsp-workspace-folders-add "Add workspace folder")
    ("md" lsp-workspace-folders-remove "Remove workspace folder")
    ("ms" lsp-workspace-folders-switch "Switch workspace folder")))

(use-package lsp-ui
  :defer
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (general-define-key :keymaps 'lsp-ui-peek-mode-map
                      "h" 'lsp-ui-peek--select-prev-file
                      "j" 'lsp-ui-peek--select-next
                      "k" 'lsp-ui-peek--select-prev
                      "l" 'lsp-ui-peek--select-next-file))

(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp)


(provide 'mugu-lsp)

;;; mugu-lsp ends here
