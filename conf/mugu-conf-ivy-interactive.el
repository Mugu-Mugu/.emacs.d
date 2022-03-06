;;; mugu-conf-interactive --- Summary
;; provide the configuration for interaction with emacs

;;; Commentary:
;;; Code:
(require 'use-package)
(require 'general)

(use-package mugu-space
  :straight nil
  :config
  (mugu-space-activate-ivy-menu))

(use-package ivy
  :defer
  :delight ivy-mode
  :custom
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1)
  )

(use-package mugu-ivy
  :straight nil
  :after ivy
  :config
  (mugu-ivy-mode))

(use-package counsel
  :after ivy
  :delight
  :config
  (counsel-mode +1)
  )

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

(use-package prescient
  :custom
  (prescient-history-length 400)
  (prescient-persist-mode t)

  (prescient-sort-full-matches-first t))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  :custom
  (ivy-prescient-enable-filtering nil)
  (ivy-prescient-sort-commands
   '(:not swiper
          ivy-switch-buffer
          mugu-counsel-fzf-file
          mugu-project-find-file
          counsel-fzf
          counsel-yank-pop
          mugu-orgi-headlines)))

(use-package ivy-avy
  :defer
  :straight nil
  :commands ivy-avy
  :after ivy)

(use-package ivy-posframe
  :after ivy
  :config
  (ivy-posframe-mode)
  :custom
  (posframe-inhibit-double-buffering nil)
  (ivy-posframe-display-functions-alist '((swiper . ivy-display-function-fallback)
                                          (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-height-alist '((swiper . 20)
                               (t . 20))))

(use-package mugu-ivy-posframe
  :disabled "Todo this does not work because ivy is a mess"
  :after ivy-posframe
  :config
  (mugu-ivy-posframe-mode))

(provide 'mugu-conf-ivy-interactive)
;;; mugu-conf-interactive ends here
