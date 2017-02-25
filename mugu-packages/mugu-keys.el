;;; Package --- Summary
;;; this package gathers all global keys binding that are not tied to a single
;;; package or feature
;;; Commentary:

;;; Code:

(require 'evil)
(require 'mugu-core)
(require 'mugu-menu)


(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-normal-state-map "jk" 'keyboard-quit)
(key-chord-define minibuffer-local-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-ns-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-completion-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-must-match-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-isearch-map "jk" 'minibuffer-keyboard-quit)

;; motion
;;; azerty keyboard
(define-key evil-normal-state-map (kbd "à") 'evil-beginning-of-line)
(define-key evil-motion-state-map (kbd "à") 'evil-beginning-of-line)
(define-key evil-visual-state-map (kbd "à") 'evil-beginning-of-line)
(define-key evil-normal-state-map (kbd ";") 'evil-repeat)

(define-key evil-motion-state-map (kbd "SPC") 'mugu-menu-main-menu)
(define-key evil-emacs-state-map (kbd "SPC") 'mugu-menu-main-menu)


;; super star remap
(define-key evil-motion-state-map (kbd "*") 'mugu-counsel-super-star)
(define-key evil-motion-state-map (kbd "µ") 'mugu-counsel-hyper-star)

(provide 'mugu-keys)
;;; mugu-keys ends here
