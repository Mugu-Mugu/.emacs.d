;;; mugu-conf-sql --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package sqlformat :defer
  :custom
  (sqlformat-command 'sqlformat)
  (sqlformat-args '("-s2"))
  :hook
  (sql-mode . sqlformat-buffer))

(use-package sql :defer
  :straight nil
  :general
  (:keymaps 'mugu-sql-mode-map
            [remap mugu-lang-format-buffer] #'sqlformat-buffer)
  :hook
  (sql-mode . mugu-sql-minor-mode)
  :config
  (mugu-define-lang-mode "sql" ""))

(provide 'mugu-conf-sql)
;;; mugu-conf-sql ends here
