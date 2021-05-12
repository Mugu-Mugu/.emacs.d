;;; mugu-conf-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'mugu-hydra)
(require 'mugu-menu)
(require 'mugu-dumbjump)

(use-package rvm
  :after ruby-mode)

(use-package rspec-mode
  :hook ruby-mode
  :diminish
  :mode-hydra
  (rspec-compilation-mode
   (:title (with-octicon "ruby" "Rspec") :color blue :hint nil)
   ("commands"
    (("d" inf-ruby-switch-from-compilation "switch to debug"))))
  :custom
  (rspec-use-rvm t)
  (rspec-use-docker-when-possible t)
  (rspec-docker-command "docker-compose exec")
  (compilation-scroll-output t))

(use-package inf-ruby
  :after ruby-mode
  :custom
  (inf-ruby-console-environment "development")
  :config
  (inf-ruby-switch-setup)
  :pretty-hydra
  (mugu-ruby-inf-menu (:color blue :hint nil)
   ("console"
    (("n" inf-ruby-console-auto "create console")
     ("c" ruby-switch-to-inf "switch to ruby console"))
    "send"
    (("sd" ruby-send-definition "current definition")
     ("sr" ruby-send-region "current region"))
    "send and go"
    (("gd" ruby-send-definition-and-go "current definition")
     ("gr" ruby-send-region-and-go "current region")))))

(use-package mugu-ruby
  :straight nil
  :general
  (:keymaps 'mugu-ruby-minor-mode-map
            [remap mugu-lang-goto-def] (lambda () (interactive) (with-dump-jump-fallback (robe-jump)))
            [remap mugu-lang-format-buffer] #'mugu-ruby-prettify
            [remap mugu-lang-test-file] #'rspec-verify
            [remap mugu-lang-test-rerun-last] #'rspec-rerun
            [remap mugu-lang-test-toggle-goto] #'mugu-ruby-toggle-spec-and-target
            [remap mugu-lang-test-method] #'mugu-ruby-verify-method
            [remap mugu-lang-test-single-at-point] #'rspec-verify-single
            [remap mugu-lang-test-all-in-project] #'rspec-verify-all
            [remap mugu-lang-test-directory] #'mugu-ruby-rspec-verify-directory
            [remap mugu-lang-test-debugger] #'inf-ruby
            [remap mugu-lang-additional-menu] #'mugu-ruby-inf-menu/body)
  :hook
  (ruby-mode . mugu-ruby-minor-mode)
  :config
  (mugu-window-configure-side-window "\\*rspec-compilation\\*" 'top 0.7)
  (mugu-window-configure-side-window "*rails*" 'top 0.5))

(use-package robe
  :hook
  (ruby-mode . robe-mode)
  :config
  (push '(company-robe :with company-yasnippet) company-backends))

(use-package docker-robe
  :disabled
  :straight (:host github :repo "aki2o/emacs-docker-robe"))

(provide 'mugu-conf-ruby)
;;; mugu-conf-ruby ends here
