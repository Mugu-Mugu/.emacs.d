(use-package flycheck
  :ensure
  ;;; not required at start but still eventually required and can not be activated automatically unless
  ;;; a hook is set up for each major mode. Easier to put a relatively large timer for defered loading.
  :defer 3
  :config
  (global-flycheck-mode +1)
  (customize-set-value 'flycheck-display-errors-delay 0.2)
  (customize-set-value 'flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (customize-set-value 'flycheck-idle-change-delay 5)
  (setq-default flycheck-disabled-checkers (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers))
  (after 'hydra
    (defhydra mugu-lint-menu
      (:color red :hint nil :idle 0.1)
      "
[Flycheck zone]
Error Mng     : [_j_] next error     [_k_] previous error   [_l_] list all errors [_y_] extract errors
Buffer Action : [_v_] verify buffer  [_p_] purge buffer     [_c_] compile buffer
Checker Mng   : [_s_] select checker [_d_] describe checker [_x_] disable checker
About         :[_am_] manual        [_ah_] local help      [_av_] version        [_ac_] check setup
"
      ("v" flycheck-buffer)
      ("p" flycheck-clear)
      ("c" flycheck-compile)
      ("j" flycheck-next-error)
      ("k" flycheck-previous-error)
      ("l" flycheck-list-errors)
      ("y" flycheck-copy-errors-as-kill)
      ("s" flycheck-select-checker)
      ("d" flycheck-describe-checker)
      ;("h" flycheck-display-error-at-point)
      ;("e" flycheck-explain-error-at-point)
      ("ah" Display-local-help)
      ("am" flycheck-manual)
      ("av" flycheck-version)
      ("ac" flycheck-verify-setup)
      ("x" flycheck-disable-checker)
      ("q" nil "cancel hydra" :color blue)
      )
    )
  )

(use-package flycheck-pos-tip
  :ensure
  :after flycheck
  :config
  (flycheck-pos-tip-mode)
 )

(provide 'mugu-lint)
;;; mugu-lint ends here
