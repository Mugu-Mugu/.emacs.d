(use-package perspective
  :ensure perspective
  :defer
  :commands persp-switch
  :config (progn
            (persp-mode)
            (ivy-mode +1)
            (require 'mugu-directory-fix)
            (persp-make-variable-persp-local 'mugu-directory-path)
            )
  )

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
