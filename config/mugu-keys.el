;;; this package gathers all global keys binding to ensure their coherency

(require 'evil)
(require 'mugu-core)
(require 'mugu-menu)

(define-key evil-motion-state-map (kbd "SPC") 'mugu-menu-main-hydra/body)

(provide 'mugu-keys)
