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
  (:keymaps 'mugu-ruby-minor-mode-map
            [remap mugu-lang-format-buffer] #'mugu-ruby-prettify
            [remap mugu-lang-test-file] #'rspec-verify
            [remap mugu-lang-test-rerun-last] #'rspec-rerun
            [remap mugu-lang-test-toggle-goto] #'mugu-ruby-toggle-spec-and-target
            [remap mugu-lang-test-method] #'mugu-ruby-verify-method
            [remap mugu-lang-test-single-at-point] #'rspec-verify-single
            [remap mugu-lang-test-all-in-project] #'rspec-verify-all
            [remap mugu-lang-goto-def] #'dumb-jump-go)
  :hook
  (ruby-mode . mugu-ruby-minor-mode)
  :config
  (mugu-window-configure-side-window "\\*rspec-compilation\\*" 'top 0.7))


(use-package robe
  :disabled "does not work with docker file"
  :hook
  (ruby-mode . robe-mode))

(use-package docker-robe
  :disabled
  :straight (:host github :repo "aki2o/emacs-docker-robe"))

(provide 'mugu-conf-ruby)
;;; mugu-conf-ruby ends here
