;;; mugu-py --- Summary
;; tbc
;;; Commentary:

;;; Code:
;; For elpy
(require 'use-package)

;; (use-package unittest-mode
;;   :hook
;;   (python-mode . unittest-mode)
;;   :config
;;   (defhydra mugu-python-unittest-hydra (:color blue :docstring nil)
;;     "Hydra for python unittest."
;;     ("tt" unittest-execute-current-file "execute current file")
;;     ("tm" unittest-run-test-case "execute current test")
;;     ("ta" )
;;     )
;;   )

(use-package python-mode
  :straight nil
  :hook
  (python-mode . lsp)
  (python-mode . flycheck-mode)
  :init
  (mugu-menu-register-mode-menu 'python-mode 'mugu-lsp-menu)
  :config)

(use-package elpy
  :defer t
  :disabled
  :config
  (elpy-enable))

;; (defhydra mugu-py-main-hydra
;;   (:color blue :hint nil)
;;   "
;;                                 -- PY MENU --
;; "
;;   ("h" elpy-nav-backward-indent "down indent" :column "1-Navigation" :color red)
;;   ("j" elpy-nav-forward-block "down block" :color red)
;;   ("k" elpy-nav-backward-block "up block" :color red)
;;   ("l" elpy-nav-forward-indent "up indent" :color red)
;;   ("M-h" elpy-nav-indent-shift-left "< indent" :column "2-Move" :color red)
;;   ("M-j" elpy-nav-move-line-or-region-down "move down" :color red)
;;   ("M-k" elpy-nav-move-line-or-region-up "move up" :color red)
;;   ("M-l" elpy-nav-indent-shift-right "> indent" :color red)
;;   ("g" elpy-goto-definition "goto def of symbol at point" :column "3-xref")
;;   ("f" xref-find-references "find xref at point")
;;   ("s" xref-find-apropos    "find xref matching pattern")
;;   ("F" elpy-format-code     "reformat region or buffer" :column "4-misc")
;;   ("d" elpy-doc             "display documentation at point"))

(provide 'mugu-py)
;;; mugu-py ends here
