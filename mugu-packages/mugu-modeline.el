(use-package telephone-line
  :ensure
  :disabled
  :after 'evil
  :config (progn
            
            (setq telephone-line-lhs
                  '((evil   . (telephone-line-evil-tag-segment))
                    (accent . (telephone-line-vc-segment
                               telephone-line-erc-modified-channels-segment
                               telephone-line-process-segment))
                    (nil    . (telephone-line-minor-mode-segment
                               telephone-line-buffer-segment))))
            (setq telephone-line-rhs
                  '((nil    . (telephone-line-misc-info-segment))
                    (accent . (telephone-line-major-mode-segment))
                    (evil   . (telephone-line-airline-position-segment))))
            (telephone-line-mode 1)
            (setq frame-title-format '("emacs@"mugu-directory-path))
            )
  )

(use-package spaceline
  :ensure
  :demand
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t)
  (spaceline-spacemacs-theme))


(provide 'mugu-modeline)