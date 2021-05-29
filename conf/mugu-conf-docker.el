;;; mugu-conf-docker --- Docker helpers -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package dockerfile-mode
  :defer
  :hook
  (dockerfile-mode . lsp)
  :pretty-hydra
  (mugu-dockerfile-mode-menu
   (:title (with-fileicon "dockerfile" "Dockerfile") :color blue :hint nil)
   ("Commands"
    (("b" dockerfile-build-buffer "build")
     ("B" dockerfile-build-buffer-no-cache "build without cache"))))
  :general
  (:keymaps '(dockerfile-mode-map)
            [remap mugu-lang-compile] #'dockerfile-build-buffer
            [remap mugu-lang-additional-menu] #'mugu-dockerfile-mode-menu/body
            [remap mugu-menu-call-mode-menu] #'mugu-lang-menu)
  :config
  (mugu-window-configure-side-window "\\*docker-build-output:.*" 'top 0.4)
  (mugu-lsp-activate-for-keymap 'dockerfile-mode-map)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker
  :defer
   :general
   (:keymaps '(docker-container-mode)
             [remap mugu-menu-call-mode-menu] #'docker-containers))


(provide 'mugu-conf-docker)
;;; mugu-conf-docker ends here
