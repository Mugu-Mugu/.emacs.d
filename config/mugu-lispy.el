(use-package lispy
  :ensure
  :diminish lispy-mode 
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package evil-lispy
  :ensure
  :after 'lispy
  :diminish evil-lispy-mode lispy-other-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'clojure-mode-hook #'evil-lispy-mode)
  :config
  (key-chord-define evil-lispy-state-map "jk" 'evil-normal-state)
  (define-key lispy-mode-map  (kbd "C-&") #'lispy-describe-inline)
  (define-key lispy-mode-map  (kbd "C-é") #'lispy-arglist-inline) 
  (evil-define-key 'insert evil-lispy-mode-map
    (kbd "C-&") #'lispy-describe-inline
    (kbd "C-é") #'lispy-arglist-inline))

(provide 'mugu-lispy)

