;;; mugu-rust --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package rustic
  :defer)

(use-package mugu-rust
  :straight nil
  :defer
  :hook
  (rustic-mode . mugu-rust-minor-mode)
  :custom
  (rustic-doc-search-function #'mugu-rust-doc-search)
  :config
  (mugu-lsp-activate-for-keymap 'mugu-rust-minor-mode-map)
  :general
  (:keymaps 'mugu-rust-minor-mode-map
            [remap mugu-lang-additional-menu] #'mugu-rust-additional-menu/body
            [remap mugu-lang-compile] #'rustic-compile
            [remap mugu-lang-doc-search] #'mugu-rust-doc-search
            [remap mugu-lang-doc-toc] #'mugu-rust-doc-toc
            [remap mugu-lang-test-file] #'rustic-cargo-test-run
            [remap mugu-lang-test-single-at-point] #'rustic-cargo-current-test)

  :pretty-hydra
  (mugu-rust-additional-menu
   (:color blue :hint nil)
   ("Cargo"
    (("ca" rustic-cargo-add "rustic-cargo-add")
     ("crm" rustic-cargo-rm "rustic-cargo-rm")
     ("cu" rustic-cargo-upgrade "rustic-cargo-upgrade"))
    "Misc"
    (("p" rustic-popup "various command submenu")))))

(use-package rustic
  :defer
  :after mugu-window
  :config
  (mugu-window-configure-side-window "cargo" 'top 0.3)
  (mugu-window-configure-side-window "*rustic-compilation*" 'top 0.3))

(provide 'mugu-conf-rust)
;;; mugu-conf-rust ends here
