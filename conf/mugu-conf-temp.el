;;; mugu-conf-temp --- Playground to test packages before integration or not
;; tbc
;;; Commentary:
(require 'use-package)

;; to move elsewhere
(use-package yaml-mode :defer)
(use-package web-mode :defer)
(use-package wgrep :defer)
(use-package restclient :defer)
(use-package ivy-posframe
  :defer
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq posframe-inhibit-double-buffering t)
  (ivy-posframe-mode))
;;; Code:

(provide 'mugu-conf-temp)
;;; mugu-conf-temp ends here
