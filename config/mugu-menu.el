;;; goal of this package is to provide a global menu for common function such as open file, change dir etc...
;;; the main menu doesnt verify if bound features are present as they are meant to be loaded lazily
;;; a binding (double SPC) is reserved for major mode submenu
(require 'hydra)

;:color
;| color    | toggle                     |
;|----------+----------------------------|
;| red      |                            |
;| blue     | :exit t                    |
;| amaranth | :foreign-keys warn         |
;| teal     | :foreign-keys warn :exit t |
;| pink     | :foreign-keys run          |

(defvar mugu-menu-mode-menus (list)
  "association list between a major mode and a menu"
  )

(defun mugu-menu-stub-mode-menu ()
  "placeholder menu that does nothing but display a message"
  (interactive)
  (message "No menu registered for this mode [%s]" major-mode)
  )

(defun mugu-menu-call-mode-menu ()
  "This function will display the menu applicable for the current mode or do 
nothing but display a message if no menu has been registered for this mode"
  (interactive)
  (call-interactively (alist-get major-mode mugu-menu-mode-menus #'mugu-menu-stub-mode-menu)))

(defun mugu-menu-register-mode-menu (mode-symbol menu-function)
  "Bind a menu MENU-FUNCTION to the mode MODE-SYMBOL. This menu may be called
at user request for the bound mode" 
  (add-to-list 'mugu-menu-mode-menus `(,mode-symbol . ,menu-function) nil 'eq))

(defun mugu-hydra-switch-buffer ()
  (interactive)
  (ivy-switch-buffer))

 (defhydra mugu-menu-main-hydra
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
  ("h" mugu-menu-help-hydra/body)
  ("d" mugu-directory-with-current-file-path "cd to current file" :color red)
  ("!" mugu-lint-menu/body)
  ("o" mugu-org-main-menu/body)
  ("q" nil "cancel hydra" :color blue)
  ("SPC" mugu-menu-call-mode-menu "mode custom binding"))

(defhydra mugu-menu-help-hydra (:color teal
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

(defalias 'mugu-menu-main-menu 'mugu-menu-main-hydra/body)
(defalias 'mugu-menu-help-menu 'mugu-menu-help-hydra)

(provide 'mugu-menu)
