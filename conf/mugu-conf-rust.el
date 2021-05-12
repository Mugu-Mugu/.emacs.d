;;; mugu-rust --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package rustic :defer)

(use-package mugu-rust
  :straight nil
  :defer
  :hook
  (rustic-mode . mugu-rust-minor-mode)
  :config
  (mugu-lsp-activate-for-keymap 'mugu-rust-minor-mode-map)
  :general
  (:keymaps 'mugu-rust-minor-mode-map
            [remap mugu-lang-additional-menu] #'mugu-rust-additional-menu/body
            [remap mugu-lang-test-file] #'rustic-cargo-test-run
            [remap mugu-lang-test-single-at-point] #'rustic-cargo-current-test)

  :pretty-hydra
  (mugu-rust-additional-menu (:color blue :hint nil))
  ("Cargo"
   (("ca" rustic-cargo-add "rustic-cargo-add")
    ("crm" rustic-cargo-rm "rustic-cargo-rm")
    ("cu" rustic-cargo-upgrade "rustic-cargo-upgrade"))
   "Misc"
   (("p" rustic-popup "various command submenu")))
)

(provide 'mugu-conf-rust)
;;; mugu-rust ends here
