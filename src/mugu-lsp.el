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
(require 'dumb-jump)
(require 'mugu-dumbjump)



(defmenu mugu-lsp-menu (:color red :hint nil)
  "-- Language menu -- "

  ("sD" lsp-disconnect "disconnect" :column "Session")
  ("sd" lsp-describe-session "describe session")
  ("sq" lsp-workspace-shutdown "shutdown server")
  ("sr" lsp-workspace-restart "restart server")
  ("ss" lsp "start server")

  ("fa" lsp-workspace-folders-add "add folder" :column "Folder")
  ("fb" lsp-workspace-blacklist-remove "un-blacklist folder")
  ("fr" lsp-workspace-folders-remove "remove folder")

  ("Td" lsp-modeline-diagnostics-mode "toggle modeline diagnostics" :column "Misc Toggles")
  ("Tl" lsp-toggle-trace-io "toggle log io")
  ("Tt" lsp-treemacs-sync-mode "toggle treemacs integration")

  ("ts" lsp-ui-sideline-mode "toggle sideline" :column "UI toggles")
  ("ta" lsp-modeline-code-actions-mode "toggle modeline code actions")
  ("tb" lsp-headerline-breadcrumb-mode "toggle breadcrumb")
  ("td" lsp-ui-doc-mode "toggle documentation popup")
  ("tf" lsp-toggle-on-type-formatting "toggle on type formatting")
  ("th" lsp-toggle-symbol-highlight "toggle highlighting")
  ("tl" lsp-lens-mode "toggle lenses")
  ("ts" lsp-toggle-signature-auto-activate "toggle signature")
  ("q" nil "quit" :color blue :column nil))

;;;###autoload
(defun mugu-lsp-activate-for-keymap (keymap-sym)
  "Configure lsp binding for the given KEYMAP-SYM symbol."
  (general-define-key :keymaps keymap-sym
                      [remap mugu-lang-format-buffer] #'lsp-format-buffer
                      [remap mugu-lang-format-region] #'lsp-format-region
                      [remap mugu-lang-lsp-menu] #'mugu-lsp-menu
                      [remap mugu-lang-doc-show-at-point] #'mugu-lsp-doc-show-and-focus
                      [remap mugu-lang-find-declaration] #'lsp-find-declaration
                      [remap mugu-lang-find-definition] #'lsp-find-definition
                      [remap mugu-lang-find-implementation] #'lsp-find-implementation
                      [remap mugu-lang-find-reference] #'lsp-find-references
                      [remap mugu-lang-find-symbol] #'xref-find-apropos
                      [remap mugu-lang-find-type-definition] #'lsp-find-type-definition
                      [remap mugu-lang-rename-thing] #'lsp-rename
                      [remap mugu-lang-execute-code-action] #'lsp-execute-code-action
                      [remap mugu-lang-peek-find-definitions] #'lsp-ui-peek-find-definitions
                      [remap mugu-lang-peek-find-implementation] #'lsp-ui-peek-find-implementation
                      [remap mugu-lang-peek-find-references] #'lsp-ui-peek-find-references
                      [remap mugu-lang-organize-imports] #'lsp-organize-imports
                      [remap mugu-lang-peek-find-workspace-symbol] #'lsp-ui-peek-find-workspace-symbol))

(defun mugu-lsp-doc-show-and-focus ()
  "."
  (interactive)
  (lsp-ui-doc-show)
  (lsp-ui-doc-focus-frame))

(defun mugu-lsp-doc-hide ()
  "."
  (interactive)
  (lsp-ui-doc-unfocus-frame)
  (lsp-ui-doc-hide))

(defun mugu-lsp-activate-ui-keymap ()
  "."
  (general-define-key :keymaps 'lsp-ui-peek-mode-map
                      "h" 'lsp-ui-peek--select-prev-file
                      "j" 'lsp-ui-peek--select-next
                      "k" 'lsp-ui-peek--select-prev
                      "l" 'lsp-ui-peek--select-next-file))

(provide 'mugu-lsp)
;;; mugu-lsp ends here
