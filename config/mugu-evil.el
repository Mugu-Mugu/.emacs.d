(use-package key-chord
  :ensure key-chord
  :config (progn
            (key-chord-mode t)
            (setq key-chord-two-keys-delay 0.1)))

;;; loading package
(use-package evil
  :ensure
  :diminish undo-tree-mode
  :config
  (evil-mode +1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "jk" 'keyboard-quit)
  (key-chord-define minibuffer-local-map "jk" 'minibuffer-keyboard-quit)
  (key-chord-define minibuffer-local-ns-map "jk" 'minibuffer-keyboard-quit)
  (key-chord-define minibuffer-local-completion-map "jk" 'minibuffer-keyboard-quit)
  (key-chord-define minibuffer-local-must-match-map "jk" 'minibuffer-keyboard-quit)
  (key-chord-define minibuffer-local-isearch-map "jk" 'minibuffer-keyboard-quit)
  ;;; motion
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-timer)
  (define-key evil-motion-state-map (kbd "s") 'avy-goto-char-timer)
  (define-key evil-visual-state-map (kbd "s") 'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "f") 'ace-window)
  (define-key evil-motion-state-map (kbd "f") 'ace-window)
  (define-key evil-visual-state-map (kbd "f") 'ace-window)
  (define-key evil-normal-state-map (kbd "à") 'evil-beginning-of-line)
  (define-key evil-motion-state-map (kbd "à") 'evil-beginning-of-line)
  (define-key evil-visual-state-map (kbd "à") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd ";") 'evil-repeat)
  )

;; super escape
;; to improve 
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

(provide 'mugu-evil)
