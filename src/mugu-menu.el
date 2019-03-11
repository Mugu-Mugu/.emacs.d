;;; mugu-menu --- Summary
;; goal of this package is to provide a global menu for common function such as
;; open file, change dir etc...  the main menu doesnt verify if bound features
;; are present as they are meant to be loaded lazily a binding (double SPC) is
;; reserved for major mode submenu
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'mugu-directory)
(require 's)
(require 'dash)

(defmacro defmenu (name body &optional docstring &rest heads)
  "Same as `defhydra' but naming is different and args are recorded for replay.
Will create a hydra NAME-hydra as well as a direct alias NAME-menu and NAME.
All arguments NAME BODY DOCSTRING and HEADS are recorded then passed as is to
`defhydra'.
This recording allows hydra replaying and enable some dynamic behavior."
  (declare (indent defun) (doc-string 3))
  (let* ((hydra-name (format "%s-hydra" (s-replace "/" "-" (symbol-name name))))
         (hydra-sym (intern hydra-name))
         (hydra-body-sym (intern (format "%s/body" hydra-name)))
         (menu-sym (intern (format "%s-menu" name))))
    `(progn
       (defhydra ,hydra-sym ,body ,docstring ,@heads)
       (defvar ,(intern (format "%S-hydra/body" name)) ',body
         "BODY argument that was used for this hydra generation")
       (defvar ,(intern (format "%S-hydra/docstring" name)) ',docstring
         "DOSTRING argument that was used for this hydra generation")
       (defalias ',menu-sym ',hydra-body-sym)
       (defalias ',name ',hydra-body-sym))))

(defun mugu-menu-add-entries (name &rest heads)
  "Wrapper around `mugu-hydra-add-head'.
NAME is as NAME in defmenu, the real name of the menu will be retrieved.
HEADS is a list of head expected to be understood by `defhydra'."
  (let* ((hydra-name (format "%s-hydra" (s-replace "/" "-" (symbol-name name))))
         (hydra-sym (intern hydra-name)))
    (apply #'mugu-hydra-add-head hydra-sym heads)))

(defvar mugu-menu-mode-menus (list)
  "Association list between a major mode and a menu.")

(defun mugu-menu-stub-mode-menu ()
  "Placeholder menu that does nothing but display a message."
  (interactive)
  (message "No menu registered for this mode [%s]" major-mode))

(defun mugu-menu-call-mode-menu ()
  "This function will display the menu applicable for the current mode.
If no menu has been registered for the registered for this mode"
  (interactive)
  (call-interactively (alist-get major-mode mugu-menu-mode-menus #'mugu-menu-stub-mode-menu)))

(defun mugu-menu-register-mode-menu (mode-symbol menu-function)
  "Bind a menu MENU-FUNCTION to the mode MODE-SYMBOL.
This menu may be called at user request for the bound mode"
  (add-to-list 'mugu-menu-mode-menus `(,mode-symbol . ,menu-function) nil 'eq))

(defun mugu-menu-register-permanent-menu (head)
  "Add given HEAD to the main menu.
This HEAD will be permanently available in the main menuan gathered in a
specific column"
  (require 'mugu-hydra)
  (hydra--head-set-property head :column "7-Submenu")
  (mugu-hydra-add-head 'mugu-menu-main-hydra head))


(defun mugu-menu-select-method (&rest methods)
  "Return the first method of METHODS that is defined."
  (-first-item
   (-non-nil
    (--map (when (fboundp it) it)
           methods))))

(defun mugu-menu-switch-buffer ()
  "Switch to buffer using the most appropriate method."
  (interactive)
  (call-interactively (mugu-menu-select-method 'mugu-project-switch-buffer-global
                                               'ivy-switch-buffer
                                               'switch-to-buffer)))

(provide 'mugu-menu)
;;; mugu-menu ends here
