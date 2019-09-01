;;; mugu-lsp --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'general)
(require 'mugu-menu)
(require 'lsp)
(require 'lsp-ui)
(require 'mugu-lang)

(defmenu mugu-lsp-menu (:color blue :hint nil :inherit (mugu-lang-menu-hydra/heads))
  "-- Language menu -- "
  ("dd" lsp-ui-doc-mode "display doc" :column "Display" :color red)
  ("ds" lsp-ui-sideline-toggle-symbols-info "display symbol" :color red)
  ("md" lsp-describe-session "describe lsp session" :column "Management")
  ("mr" lsp-workspace-restart "Restart project")
  ("ma" lsp-workspace-folders-add "Add workspace folder")
  ("mD" lsp-workspace-folders-remove "Remove workspace folder")
  ("ms" lsp-workspace-folders-open "Switch workspace folder"))

(defun mugu-lsp-activate-for-keymap (keymap-sym)
  "Configure lsp binding for the given KEYMAP-SYM symbol."
  (general-define-key :keymaps keymap-sym
                      [remap mugu-lang-goto-def] #'lsp-find-definition
                      [remap mugu-lang-find-ref] #'lsp-ui-peek-find-references
                      [remap mugu-lang-execute-code-action] #'lsp-execute-code-action
                      [remap mugu-lang-format-buffer] #'lsp-format-buffer
                      [remap mugu-lang-rename-thing] #'lsp-rename
                      [remap mugu-menu-call-mode-menu] #'mugu-lsp-menu))

(defun mugu-lsp-activate-ui-keymap ()
  "."
  (general-define-key :keymaps 'lsp-ui-peek-mode-map
                      "h" 'lsp-ui-peek--select-prev-file
                      "j" 'lsp-ui-peek--select-next
                      "k" 'lsp-ui-peek--select-prev
                      "l" 'lsp-ui-peek--select-next-file))

(provide 'mugu-lsp)
;;; mugu-lsp ends here
