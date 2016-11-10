;;; goal of this package is to provide a global menu for common function such as open file, change dir etc...
;;; the main menu doesnt verify if bound features are present as they are meant to be loaded lazily
;;; a binding (double SPC) is reserved for major mode submenu
(require 'hydra)
(require 'mugu-directory-fix)

;:color
;| color    | toggle                     |
;|----------+----------------------------|
;| red      |                            |
;| blue     | :exit t                    |
;| amaranth | :foreign-keys warn         |
;| teal     | :foreign-keys warn :exit t |
;| pink     | :foreign-keys run          |

(defvar mugu-menu-mode-menus (list)
  "association list between a major mode and a menu")

(defun mugu-menu-stub-mode-menu ()
  "placeholder menu that does nothing but display a message"
  (interactive)
  (message "No menu registered for this mode [%s]" major-mode))

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

(defhydra mugu-menu-main-hydra (:color blue :hint nil)
  "
MAIN MENU :
  -> File    Dir : %s(mugu-directory-pwd-file)
  -> Current Dir : %s(mugu-directory-pwd)
" 
  ("b" mugu-hydra-switch-buffer "buffer" :column "1-Switch") 
  ("m" counsel-recentf "recent")
  ("f" (with-mugu-dir 'counsel-find-file) "file current dir")
  ("r" (with-mugu-dir 'counsel-rg-find-file-recursive) "file recursively")
  ("y" counsel-yank-pop "yank ring" :column "2-find")
  ("g" swiper "swiper")
  ("s" counsel-semantic "semantic")
  ("x" counsel-M-x "execute" :column "3-Others")
  ("cd" (with-mugu-dir 'cd) "change dir")
  ("d" mugu-directory-with-current-file-path "cd to current file" :color red)
  ("u" counsel-unicode-char "insert unicode")
  ("SPC" mugu-menu-call-mode-menu "major mode" :column "4-Submenu")
  ("w" mugu-workspace-hydra-menu/body "workspace")
  ("p" mugu-project-hydra-menu/body "project")
  ("h" mugu-menu-help-hydra/body "help")
  ("!" mugu-lint-menu/body "linting")
  ("o" mugu-org-main-menu/body "orgmode")
  ("q" nil "cancel hydra" :color blue :column nil))

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
