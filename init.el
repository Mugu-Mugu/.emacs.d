(add-to-list 'load-path (concat user-emacs-directory "site_specific"))
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/eyecandy"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/languages"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/hydra"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'mugu-proxy)

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;;; main emacs config here

(global-hl-line-mode t)
(set-face-background 'hl-line "#3e4446")
;; Show parentheses
(show-paren-mode 1)
;; highlight entire expression when matching paren is not visible;
;; otherwise just highlight matching paren
(setq show-paren-style 'mixed)
(setq whitespace-style '(trailing))
(global-whitespace-mode 1)

; to rework
(require 'mugu-core)

  ;:config (my-themes-load-default))

;(use-package my-powerline)

; To gather and configure all packages relevant to sessions persistance
(use-package mugu-sessions-persistance)

; To configure helm
;(use-package my-helm-core)

; To gather and configure direct package extension to helm
;(use-package my-helm-extensions)

; Custom package to provide vim unite tabe like feature by combining perspective, projectile and helm
(use-package mugu-tabs)

(use-package key-chord
  :ensure key-chord
  :defer t
  :config (progn
            (key-chord-mode t)
            (setq key-chord-two-keys-delay 0.1)))


;hydra should load helm/projectile/tabs

;(require 'my-hydra)
;(require 'my-hydra-custom)
(use-package mugu-hydra)
(use-package mugu-helm)
(use-package mugu-modeline)
(use-package mugu-motion)
(use-package mugu-completion)
(use-package mugu-lisp)
(use-package mugu-directory-fix)
;(use-package mugu-lispy)


;(defvar mugu-directory "C:/DAM")
;(defun mugumugumugu () (progn
;            (setq default-directory mugu-directory)))
;
;(add-hook 'buffer-list-update-hook 'mugumugumugu)
;(add-hook 'eshell-directory-change-hook 'mugumugumugu)

;(require 'my-bookmarks)

;(require 'my-buffers)
;(require 'uniquify)
;(use-package ibuffer

;(require 'my-elisp)

;(require 'my-tags)
;(use-package ggtags

;(require 'my-autocomplete)

;(require 'my-ido)
;(require 'my-projectile-persp)
;(require 'my-ag)
;(require 'my-interaction)
;(require 'my-flycheck)
;(require 'my-woman)
;(require 'my-comint)
;(require 'my-unbound-keys)
;(require 'my-languages)
;(require 'my-shell)
;(require 'my-filetypes)
;(require 'my-term)
;(require 'my-magit)
;(require 'my-android)
;(require 'my-eshell)
;(require 'my-ielm)
;(require 'my-package-list)
;(require 'my-evil)
;(require 'my-help)
;(require 'my-god)
;(require 'my-sessions)
;(require 'my-leader-keys)

; To gather all downloaded available theme
(use-package mugu-themes)

;;; loading package
(use-package evil
  :ensure
  :config
  (evil-mode +1))

;(diminish 'whitespace-mode)
;(diminish 'whitespace-mode)
;(diminish 'abbrev-mode)
;(diminish 'auto-revert-mode)
;(diminish 'compilation-in-progress)
;(diminish 'compilation-shell-minor-mode)
;(diminish 'global-whitespace-mode)
;(diminish 'helm-migemo-mode)
;(diminish 'isearch-mode)
;(diminish 'override-global-mode)
;(diminish 'projectile-mode)
;(diminish 'semantic-minor-modes-format)
;(diminish 'undo-tree-mode)
;(diminish 'visible-mode)
;(diminish 'whitespace-mode)
;(diminish 'auto-fill-function)
;(diminish 'auto-revert-tail-mode)
;(diminish 'compilation-minor-mode)
;(diminish 'defining-kbd-macro)
;(diminish 'global-auto-revert-mode)
;(diminish 'global-whitespace-newline-mode)
;(diminish 'next-error-follow-minor-mode)
;(diminish 'overwrite-mode)
;(diminish 'semantic-highlight-edits-mode)
;(diminish 'semantic-show-unmatched-syntax-mode)
;(diminish 'undo-tree-visualizer-selection-mode)
;(diminish 'visual-line-mode)
;(diminish 'whitespace-newline-mode)

;;; pluggin mode activation
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun minibuffer-exit-or-unite ()
  (interactive)
  (if (helm-alive-p)
      (helm-like-unite/body)
    (minibuffer-keyboard-quit)))

;; super escape
;; to improve 
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-normal-state-map "jk" 'keyboard-quit)
(key-chord-define minibuffer-local-map "jk" 'minibuffer-exit-or-unite)
(key-chord-define minibuffer-local-ns-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-completion-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-must-match-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-isearch-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define ivy-minibuffer-map "jk" 'minibuffer-keyboard-quit)


;;; evil settings mapping
;(define-key evil-motion-stakTE-map (kbd "C-h") 'evil-window-left)
;(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
;(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
;(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)



;(defvar autosave-dir "user-ema")
;(setq org-agenda-custom-commands
;      '(("cx" "TODOs sorted by state, priority, effort"
;         tags-todo "emacs+TODO=\"TODO\""
;         ((org-agenda-overriding-header "\n Emacs backlog")
;          (org-tags-match-list-sublevels 'indented)
;          (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))))))
;(setq org-stuck-projects
;      '("+PROJECT" ("TODO" "ACTIVE") () ))
;(setq org-tags-exclude-from-inheritance '("PROJECT"))
;(setq org-capture-templates
;      '(
;        ("e" "emacs" entry (file+headline mugu-org-emacs-file "capture") "* %?")
;        ("v" "emacs" entry (file+headline mugu-org-vie-courante-file "capture") "* %?")
;        ("t" "emacs" entry (file+headline mugu-org-travail-file "capture") "* %?")
;        )
;      )


;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "config/" "mugu-custom.el"))
(when (file-exists-p custom-file) (load custom-file))
(provide 'init)

; ack
; ag
; chocolatay
; iedit
; yasnippete
; timeclock
; changement de couleur sur timelog
; eval defun
; revert = refrsh dans describe function
; wdired
;visual wrap t fill paragraph
; wstrim

; emacsrocks
; github arvid
; steve yegg
; jonas bernouilli
