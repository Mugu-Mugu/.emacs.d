;;; mugu-conf-temp --- Playground to test packages before integration or not
;; tbc
;;; Commentary:
(require 'use-package)

;;; Code:
;; to move elsewhere
(use-package yaml-mode :defer)
(use-package web-mode :defer)
(use-package restclient :defer)
(use-package docker :defer)

(use-package dockerfile-mode :defer
  :config
  (add-to-list 'load-path "/your/path/to/dockerfile-mode/")
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package emacs-everywhere :defer)

(provide 'mugu-conf-temp)
;;; mugu-conf-temp ends here
