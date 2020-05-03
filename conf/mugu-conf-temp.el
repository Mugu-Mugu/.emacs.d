;;; mugu-conf-temp --- Playground to test packages before integration or not
;; tbc
;;; Commentary:
(require 'use-package)

;; to move elsewhere
(use-package yaml-mode :defer)
(use-package web-mode :defer)
(use-package restclient :defer)
(use-package ivy-posframe
  :defer
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq posframe-inhibit-double-buffering t)
  (ivy-posframe-mode))

(use-package docker :defer)
(use-package sqlformat :defer
  :custom
  (sqlformat-command 'sqlformat)
  (setq sqlformat-args '("-s2"))
  :hook
  (sql-mode . sqlformat-buffer))

(use-package sql :defer
  :straight nil
  :general
  (:keymaps 'sql-mode-map
            [remap mugu-lang-format-buffer] #'sqlformat-buffer)
  :config
  (mugu-lang-activate-for-mode 'sql))

(provide 'mugu-conf-temp)
;;; mugu-conf-temp ends here
