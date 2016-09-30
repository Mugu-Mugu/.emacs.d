(use-package avy
  :ensure
  :defer
  :config (progn
            (setq avy-case-fold-search t)
            (setq avy-all-windows nil)
            (setq avy-background t)
            (setq avy-style 'at-full)
            (setq avy-keys (number-sequence ?a ?z))         
            (setq avy-timeout-seconds 0.3)
            )
  )

(use-package ace-window
  :ensure
  :defer
  :config (setq aw-keys '(?h ?j ?k ?l ?q ?s ?d ?f ?g))
  ;; todo bigger font
  )

(use-package ace-link
  :ensure
  :defer)


(provide 'mugu-motion)
