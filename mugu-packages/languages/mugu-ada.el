(use-package ada-mode
  :defer)


(use-package mugu-ada-lint
    :after mugu-lint ada-mode
    :straight (:local-repo)
    :config
;;; unregister the default ada checkers and register the custom one
    (setq-default flycheck-disabled-checkers (cons 'ada-gnat flycheck-disabled-checkers))
    (add-hook 'ada-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
    (add-to-list 'flycheck-checkers 'ada-mugu))

(provide 'mugu-ada)
