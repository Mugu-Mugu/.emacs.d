;;; mugu-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:

(use-package robe
  :defer
  :config
  (require 'mugu-menu)
  ;; (defhydra mugu-rs-main-hydra
  ;;    (:color blue :hint nil)
  ;;    "
  ;;                                 -- rust menu --
  ;; "
  ;;    ("cb" cargo-process-build "cargo build" :column "2-cargo")
  ;;    ("ct" cargo-process-test "cargo test")
  ;;    ("cr" cargo-process-run "cargo run")
  ;;    ("g" racer-find-definition "goto def of symbol at point" :column "1-xref")
  ;;    ("f" racer-describe "display documentation at point")
  ;;    ("F" rust-format-buffer "reformat region or buffer"))

  ;; (defalias 'mugu-rs-main-menu 'mugu-rs-main-hydra/body)

  ;; (mugu-menu-register-mode-menu 'rust-mode 'mugu-rs-main-menu)
  )


;; (use-package racer
;;   :after rust-mode
;;   :ensure
;;   :config
;;   (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
;;   (setq racer-rust-src-path "~/.cargo/rust/src") ;; Rust source code PATH
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   ;; (add-hook 'racer-mode-hook #'eldoc-mode)
;;   ;; fixme : too slow now (at least on windows), remove when racer no longer spawn a process on each request
;;   (setq eldoc-documentation-function nil)
;;   (setq company-idle-delay 1.0)
;;   (setq company-tooltip-idle-delay 1.0)
;;   (setq company-quickhelp-delay 1.0)
;;   (add-hook 'racer-mode-hook #'company-mode))

;; (use-package flycheck-rust
;;   :after rust-mode
;;   :ensure
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package enh-ruby-mode
  :mode (("Appraisals\\'" . enh-ruby-mode)
         ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
         ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :custom
  (enh-ruby-deep-indent-paren nil)
  (enh-ruby-hanging-paren-deep-indent-level 2))

(provide 'mugu-ruby)
;;; mugu-ruby ends here
