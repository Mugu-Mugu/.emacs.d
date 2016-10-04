(use-package telephone-line
  :ensure
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
            (defvar mugu-directory-path-for-header '(:eval mugu-directory-path))
            (telephone-line-defsegment telephone-mugu-directory-segment
              (telephone-line-trim (format-mode-line mugu-directory-path-for-header))
              )
            ;(invert-face 'header-line)
            ;(setq telephone-line-lhs
            ;      '((evil   . (telephone-mugu-directory-segment))
            ;        (accent    . nil)
            ;        (nil . nil)))
            ;(setq telephone-line-rhs
            ;      '((nil . nil)
            ;        (accent    . nil)
            ;        (evil   . (telephone-mugu-directory-segment))))
            ;(setq-default header-line-format `("%e" ,@(telephone-line--generate-mode-line)))
            )
  )


(provide 'mugu-modeline)
