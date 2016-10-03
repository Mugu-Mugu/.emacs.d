(use-package lispy
  :ensure
  :config
  (lispy-mode +1))

(use-package evil-lispy
  :ensure
  :after 'lispy)

(provide 'mugu-lispy)

