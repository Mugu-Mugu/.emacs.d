;;; mugu-conf-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'mugu-hydra)
(require 'mugu-menu)

(use-package ruby-mode
  :hook
  (ruby-mode . lsp)
  (ruby-mode . flycheck-mode))

(use-package rvm
  :defer
  :config
  (rvm-use-default))

(use-package rspec-mode
  :after ruby-mode
  :diminish
  :config
  (setq rspec-use-rvm t)
  (setq compilation-scroll-output t))

(use-package mugu-ruby
  :after ruby-mode
  :straight nil
  :config
  (mugu-ruby-activate))

(provide 'mugu-conf-ruby)
;;; mugu-conf-ruby ends here
