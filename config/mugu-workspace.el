(use-package perspective
  :ensure perspective
  :defer
  :commands persp-switch
  :config 
  (require 'mugu-directory-fix)
  (persp-mode)
  (ivy-mode +1)
  (persp-make-variable-persp-local 'mugu-directory-path)
  (after 'mugu-hydra
    ;;; switching to buffer also change perspective if needed
    (advice-add #'mugu-hydra-switch-buffer :override #'persp-switch-to-buffer)))

(after 'hydra
  (defhydra mugu-workspace-hydra-menu (:color blue
                                              :hint nil)
    "
^workspace^              ^buffer^             
_s_: switch workspace    _d_: delete buffer
_k_: kill workspace      _m_: move buffer
_r_: rename workspace
"
    ("s" persp-switch)
    ("k" persp-kill)
    ("d" persp-remove-buffer)
    ("r" persp-rename)
    ("m" persp-set-buffer)
    ("q" nil "quit menu" :color blue)
    ("SPC" hydra-main-menu/body "return to main menu"))
  )

(provide 'mugu-workspace)
