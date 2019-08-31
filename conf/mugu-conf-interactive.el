;;; mugu-conf-interactive --- Summary
;; provide the configuration for interaction with emacs

;;; Commentary:
;;; Code:
(require 'use-package)
(require 'general)

(use-package smex :defer)

(use-package mugu-space
  :straight nil
  :config
  (mugu-space-activate-ivy-menu))

(use-package ivy
  :defer
  :delight ivy-mode
  :config
  (ivy-mode 1))

(use-package mugu-ivy
  :straight nil
  :after ivy
  :config
  (mugu-ivy-set-config)
  (mugu-ivy-install-new-actions)
  (mugu-ivy-install-keybinds))

(use-package counsel
  :after ivy
  :delight
  :config
  (counsel-mode +1))

(use-package swiper
  :defer
  :custom
  (swiper-action-recenter t)
  (swiper-stay-on-quit t))

(use-package mugu-counsel
  :commands
  mugu-counsel-fzf-file
  mugu-counsel-fzf-dir
  mugu-counsel-fzf-any
  mugu-counsel-super-star
  mugu-counsel-hyper-star
  mugu-counsel-cd
  mugu-counsel-describe-custom
  :defer
  :config
  (mugu-counsel-set-config)
  :straight nil)

(use-package expand-region
  :custom
  (expand-region-contract-fast-key "<backspace>")
  :config
  (general-def '(motion visual)
    "RET" 'er/expand-region))

(use-package wgrep :defer)

(use-package mugu-wgrep
  :straight nil
  :after ivy
  :config
  (mugu-wgrep-activate-conf))


(provide 'mugu-conf-interactive)
;;; mugu-conf-interactive ends here
