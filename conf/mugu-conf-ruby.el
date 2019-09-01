;;; mugu-conf-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'mugu-hydra)
(require 'mugu-menu)

(use-package rvm
  :after ruby-mode)

(use-package rspec-mode
  :hook ruby-mode
  :diminish
  :custom
  (rspec-use-rvm t)
  (rspec-use-docker-when-possible t)
  (rspec-docker-command "docker-compose exec")
  (compilation-scroll-output t))

(use-package mugu-ruby
  :straight nil
  :general
  (:keymaps 'ruby-mode-map
            [remap mugu-lang-goto-def] #'dumb-jump-go
            [remap mugu-menu-call-mode-menu] #'mugu-ruby-lang-menu)
  :config
  (mugu-ruby-activate))

(use-package robe
  :disabled "because it doesn't support multi project in same emacs version"
  :hook
  (ruby-mode . robe-mode))

(provide 'mugu-conf-ruby)
;;; mugu-conf-ruby ends here
