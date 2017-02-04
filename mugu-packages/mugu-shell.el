
(use-package shell
  :ensure
  :defer
  :bind
  (:map shell-mode-map
        ("C-x" . comint-get-next-from-history)
        ("C-a" . comint-bol-or-process-mark)
        ("C-u" . comint-kill-input)
        ("C-w" . backward-kill-word)
        ("C-c" . comint-interrupt-subjob)
        ("C-z" . comint-stop-subjob)
        ("C-\\" . comint-quit-subjob)
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
        ("C-r" . comint-previous-matching-input-from-in)
        ("M-s" . comint-next-matching-input-from-input)))

(provide 'mugu-shell)
