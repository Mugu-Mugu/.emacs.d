(use-package ada-mode
  :ensure
  :defer
  )

(use-package mugu-ada-lint
  :after ada-mode
  :config
  ;;; unregister the default ada checkers and register the custom one
  (setq-default flycheck-disabled-checkers (cons 'ada-gnat flycheck-disabled-checkers))
  (add-to-list 'flycheck-checkers 'ada-mugu))

(provide 'mugu-ada)
