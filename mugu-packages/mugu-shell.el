;;; package --- summary
;;; commentary 

(require 'mugu-core)
(use-package shell
  :ensure
  :defer
  :bind
  (:map shell-mode-map
        ("C-x" . comint-get-next-from-history)
        ("C-a" . comint-bol-or-process-mark)
        ("C-u" . comint-kill-input)
        ("C-w" . backward-kill-word)
        ("C-y" . yank)
        ("C-c" . comint-interrupt-subjob)
        ("C-z" . comint-stop-subjob)
        ("C-\\" . comint-quit-subjob)
        ("RET" . mugu-shell-send-input)
        ("C-o" . comint-delete-output)
        ("M-o" . comint-clear-buffer)
        ("C-e" . comint-show-maximum-output)
        ("C-l" . comint-dynamic-list-input-ring)
        ("C-n" . comint-next-prompt)
        ("C-p" . comint-previous-prompt)
        ("C-d" . comint-send-eof)
        ("C-s" . comint-write-output)
        ([up] . comint-previous-input)
        ([down] . comint-next-input)
        ("C-r" . comint-history-isearch-backward-regexp)
        ("M-s" . comint-next-matching-input-from-input))
  :config
  (require 'mugu-shell-utils)
  (add-hook 'shell-mode-hook 'superword-mode)

  ;; some evil adaptation as only prompt is supposed to be edited
  (after 'evil
    (add-hook 'shell-mode-hook
              (lambda ()
                (add-hook 'evil-insert-state-entry-hook 'mugu-shell-scroll-before-insert nil 'make-it-local)))

    (evil-define-key 'normal shell-mode-map "c" 'mugu-shell-change)
    (evil-define-key 'normal shell-mode-map "C" 'mugu-shell-change-line)

    ;; in shell o and O have no point
    (evil-define-key 'normal shell-mode-map "o" 'evil-append-line)
    (evil-define-key 'normal shell-mode-map "o" 'evil-append-line)))

(use-package mugu-shell-menu
  :after shell
  :config
  (mugu-menu-register-mode-menu 'shell-mode 'mugu-menu-shell-main/body))

(provide 'mugu-shell)
