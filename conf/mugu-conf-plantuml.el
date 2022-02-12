;;; mugu-conf-plantuml --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package plantuml-mode
  :defer
  :mode "\\.plantuml\\'"
  :hook
  (planuml-mode . prog-mode)
  :custom
  (plantuml-executable-path "~/.nix-profile/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package plantuml-mode
  :defer :after org
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package plantuml-mode
  :defer :after mugu-window
  :config
  (mugu-window-configure-side-window "\*PLANTUML Preview\*" 'right 0.5))

(use-package plantuml-mode
  :defer :after mugu-lang
  :hook
  (plantuml-mode . mugu-lang-mode))

(use-package mugu-plantuml-ext
  :straight nil
  :demand :after plantuml-mode
  :pretty-hydra
  (mugu-plantuml-ext-menu
   (:color blue :hint nil)
   ("Export"
    (("e" mugu-plantuml-ext-export "Export")
     ("d" mugu-plantuml-ext-export "Export"))
    ))
  :general
  ([remap mugu-lang-doc-search] #'mugu-plantuml-ext-goto-docs
   [remap mugu-lang-additional-menu] #'mugu-plantuml-ext-menu/body))

(use-package flycheck-plantuml
  :after flycheck plantuml-mode
  :functions flycheck-plantuml-setup
  :config
  (flycheck-plantuml-setup)
  :custom
  (flycheck-plantuml-executable plantuml-executable-path))

(provide 'mugu-conf-plantuml)
;;; mugu-conf-plantuml ends here
