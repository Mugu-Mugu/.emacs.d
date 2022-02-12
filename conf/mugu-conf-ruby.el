;;; mugu-conf-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'mugu-hydra)
(require 'mugu-menu)

(use-package mugu-lang
  :straight nil
  :hook
  (ruby-mode . mugu-lang-mode))

(use-package rvm
  :disabled "Obsolete thanks to nix"
  :after ruby-mode)

(use-package rspec-mode
  :hook
  (ruby-mode . rspec-mode)
  :diminish
  :mode-hydra
  (rspec-compilation-mode
   (:title (with-octicon "ruby" "Rspec") :color blue :hint nil)
   ("commands"
    (("d" inf-ruby-switch-from-compilation "switch to debug"))))
  :custom
  (rspec-use-docker-when-possible t)
  (rspec-docker-command "docker-compose exec")
  (compilation-scroll-output t))

(use-package inf-ruby
  :defer
  :custom
  (inf-ruby-console-environment "development")
  :commands inf-ruby-switch-setup
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

(use-package mugu-ruby
  :disabled "solargraph is still too slow for rails"
  :straight nil
  :hook
  (ruby-mode . lsp-mode)
  :config
  (mugu-lsp-activate-for-keymap 'mugu-ruby-minor-mode-map))

(use-package robe
  :general
  (:keymaps 'robe-mode-map
            [remap mugu-lang-find-definition] #'robe-jump
            [remap mugu-lang-doc-show-at-point] #'robe-doc
            [remap mugu-lang-doc-toc] #'robe-jump-to-module
            [remap mugu-lang-doc-search] #'robe-ask)
  :hook
  (ruby-mode . robe-mode))

(use-package docker-robe
  :disabled
  :straight (:host github :repo "aki2o/emacs-docker-robe"))

(provide 'mugu-conf-ruby)
;;; mugu-conf-ruby ends here
