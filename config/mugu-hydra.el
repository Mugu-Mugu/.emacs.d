(use-package hydra
  :demand
  :ensure hydra)

;:color
;| color    | toggle                     |
;|----------+----------------------------|
;| red      |                            |
;| blue     | :exit t                    |
;| amaranth | :foreign-keys warn         |
;| teal     | :foreign-keys warn :exit t |
;| pink     | :foreign-keys run          |

;:timeout

;;; stub function used as hook for mode needing additionnal mapping
(defvar hydra-custom-mode-hook nil)
(defun hydra-custom-mode-hook-run ()
  (interactive)
  (run-hooks 'hydra-custom-mode-hook))
(defun hydra-internal-custom-add-hook (hydra)
  "Internal function setting the given hydra to the local buffer custom hydra mapping"
  (add-hook 'hydra-custom-mode-hook hydra nil 'local))
(defun mugu-hydra-register-mode-hook (mode-hook hydra-body)
  "Function to set a custom hydra body on an external hook (typically a major mode hook to prevent conflict)"
  (add-hook mode-hook (apply-partially #'hydra-internal-custom-add-hook hydra-body) ))

(defun mugu-hydra-switch-buffer ()
  (interactive)
  (ivy-switch-buffer))

 (defhydra mugu-hydra-menu-main
  (:color blue :hint nil :idle 0.1)
  "
^Files^                     ^Data^                ^Others^              ^Sub Menu^
^^^^^^^^-----------------------------------------------------------------------------------------------
_b_: buffer                 _y_: yank ring         _x_ : execute         _SPC_ : major mode menu
_m_: helm-mini              _S_: recursive grep    _cd_: cd              _w_   : workspace menu
_f_: find file              _g_: grep in file      _h_ : help submenu    _p_   : project menu
_r_: find file recursivly                                            ^^^^_!_   : lint menu 
                                                                   ^^^^^^_o_   : org menu 
"
  ("b" mugu-hydra-switch-buffer)
  ("m" counsel-recentf)
  ("f" counsel-find-file)
  ("y" counsel-yank-pop)
  ("S" mugu-hydra-find-pattern-recursive)
  ("g" swiper)
  ("s" counsel-semantic)
  ("x" counsel-M-x)
  ("r" counsel-rg-find-file-recursive)
  ("w" mugu-workspace-hydra-menu/body)
  ("p" mugu-project-hydra-menu/body)
  ("cd" cd)
  ("h" hydra-emacs-help/body)
  ("d" mugu-directory-with-current-file-path "cd to current file" :color red)
  ("!" mugu-lint-menu/body)
  ("o" mugu-org-main-menu/body)
  ("q" nil "cancel hydra" :color blue)
  ("SPC" hydra-custom-mode-hook-run "mode custom binding"))
(after 'evil
  (define-key evil-normal-state-map (kbd "SPC") 'mugu-hydra-menu-main/body)
  (define-key evil-motion-state-map (kbd "SPC") 'mugu-hydra-menu-main/body)
  (define-key evil-visual-state-map (kbd "SPC") 'mugu-hydra-menu-main/body))

(defhydra hydra-emacs-help (:color teal
                            :hint nil)
  "
^EMACS^             ^Helm^         
^^^^^^^^---------------------------
_m_: man           _h_: helm
_i_: info          ^ ^
_a_: apropos       ^ ^
"
  ("m" helm-man-woman)
  ("i" helm-info-find)
  ("a" helm-apropos)
  ("h" helm-documentation))

; to move in my-helm
(defhydra helm-like-unite ()
  "vim movement"
  ("?" helm-help "help")
  ("<escape>" keyboard-escape-quit "exit")
  ("q" keyboard-escape-quit "exit")
  ("<SPC>" helm-toggle-visible-mark "mark")
  ("a" helm-toggle-all-marks "(un)mark all")
  ("v" helm-execute-persistent-action)
  ("g" helm-beginning-of-buffer "top")
  ("G" helm-end-of-buffer "bottom")
  ("j" helm-next-line "down")
  ("k" helm-previous-line "up")
  ("h" helm-previous-source)
  ("l" helm-next-source)
  ("i" nil "cancel"))
(key-chord-define helm-like-unite/keymap "jk" 'hydra-keyboard-quit)

(provide 'mugu-hydra)
