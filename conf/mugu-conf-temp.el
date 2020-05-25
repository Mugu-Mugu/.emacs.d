;;; mugu-conf-temp --- Playground to test packages before integration or not
;; tbc
;;; Commentary:
(require 'use-package)

;; to move elsewhere
(use-package yaml-mode :defer)
(use-package web-mode :defer)
(use-package restclient :defer)

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
