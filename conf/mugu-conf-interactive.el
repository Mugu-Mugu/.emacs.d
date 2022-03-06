;;; mugu-conf-interactive --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package vertico
  :config
  (vertico-mode)
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  :general
  (:keymaps '(minibuffer-mode-map)
            "?" #'minibuffer-completion-help))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :config
  :general
  (:keymaps 'minibuffer-local-map
            "M-o" #'embark-act ;; pick some comfortable binding
            "C-;" #'embark-dwim ;; good alternative: M-.
            "C-h B" #'embark-bindings)) ;; alternative for `describe-bindings'

(use-package embark
  :defer :after mugu-window
  :custom
  (embark-verbose-indicator-display-action
   '(mugu-window-display-buffer-top-level-child-frame
     (window-parameters . ((mode-line-format . 'none)))
     (child-frame-parameters . ((minibuffer . nil)
                                (buffer-predicate . (lambda (buffer) (equal (buffer-name buffer) "*Embark Actions*")))
                                (left . 0.5)
                                (width . 0.7)
                                (top . 0.5))))))

(use-package consult
  :defer
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (consult-narrow-key "<")
  (xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :demand :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package mini-frame
  :config
  (mini-frame-mode)
  :custom
  (mini-frame-ignore-commands
   '(eval-expression "edebug-eval-expression" debugger-eval-expression evil-ex evil-ex-search-forward evil-ex-search-backward))
  (x-gtk-resize-child-frames 'resize-mode)
  (mini-frame-show-parameters
   '((top . 0.05)
     (width . 0.7)
     (left . 0.5))))

(use-package maple-minibuffer
  :disabled "not production ready"
  :straight (:host github :repo "honmaple/emacs-maple-minibuffer")
  :config
  (maple-minibuffer-mode  -1)

  (setq maple-minibuffer:action '(read-from-minibuffer read-string)
        maple-minibuffer:ignore-action '(evil-ex eval-expression)
        maple-minibuffer:width 0.7)

  (setq embark-verbose-indicator-display-action
        '(mugu-embark-display-buffer-actions-pop-up-frame))

  (add-to-list 'display-buffer-alist '("\\*Embark Actions\\*"
                                       (mugu-embark-display-buffer-actions-pop-up-frame)))

  (setq maple-minibuffer:position-type 'frame-top-center))

(provide 'mugu-conf-interactive)
;;; mugu-conf-interactive.el ends here
