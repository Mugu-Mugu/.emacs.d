;;; this package gathers all global keys binding to ensure their coherency

(require 'evil)
(require 'mugu-core)
(require 'mugu-menu)

(define-key evil-motion-state-map (kbd "SPC") 'mugu-menu-main-menu)
(define-key evil-emacs-state-map (kbd "SPC") 'mugu-menu-main-menu)

(provide 'mugu-keys)
