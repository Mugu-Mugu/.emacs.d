;;; mugu-menu --- Summary
;; goal of this package is to provide a global menu for common function such as
;; open file, change dir etc...  the main menu doesnt verify if bound features
;; are present as they are meant to be loaded lazily a binding (double SPC) is
;; reserved for major mode submenu
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'mugu-directory-fix)
(require 's)

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
       (defvar ,(intern (format "%S-hydra/body" name)) body
         "BODY argument that was used for this hydra generation")
       (defvar ,(intern (format "%S-hydra/docstring" name)) docstring
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
  "Bind a menu MENU-FUNCTION to the mode MODE-SYMBOL. This menu
may be called at user request for the bound mode"
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

 (defhydra mugu-menu-main-hydra (:color blue :hint nil)
  "
                               -- MAIN MENU --

  -> File    Dir : %s(mugu-directory-pwd-file)
  -> Current Dir : %s(mugu-directory-pwd)
"
  ("b"  mugu-menu-switch-buffer "previous buffer" :column "1-Change File")
  ("ff" (with-mugu-dir (counsel-find-file)) "file current dir")
  ("fr" (with-mugu-dir (find-file (mugu-counsel-find-file-recursive))) "file recursively")
  ("fa" (with-mugu-dir (find-file (mugu-counsel-find-anything-recursive))) "file recursively")
  ("fl" counsel-recentf "file recently used")
  ("fb" (find-file (mugu-counsel-read-bookmark-file)) "file bookmarked")
  ("d" mugu-directory-with-current-file-path "cd to current file" :color red :column "2-Change Dir")
  ("cd" (with-mugu-dir (call-with-fzf-matcher #'mugu-directory-cd (read-directory-name "Change dir: "))) "change dir" :color red)
  ("cr" (with-mugu-dir (mugu-directory-cd (mugu-counsel-find-dir-recursive))) "change dir recursively" :color red)
  ("cm" (mugu-directory-cd (mugu-counsel-read-bookmark-dir)) "change dir from bookmark" :color red)
  ("mm" counsel-bookmark "go/register bookmark" :column "3-Bookmark")
  ("mf" (find-file (mugu-counsel-read-bookmark-file)) "go to file bookmark")
  ("md" (mugu-directory-cd (mugu-counsel-read-bookmark-dir)) "go to directory bookmark" :color red)
  ("mr" mugu-bookmark-register-dir "register directory bookmark" :color red)
  ("ss" counsel-grep-or-swiper "swiper" :column "3-Search")
  ("sr" counsel-rg "rgrep")
  ("sg" counsel-git-grep "git grep")
  ("sx" sx-search "git grep")
  ("y" counsel-yank-pop "yank ring" :column "4-Misc")
  ("u" counsel-unicode-char "insert unicode")
  ("j" (mugu-scroll-lines 3) "scroll down" :color red)
  ("k" (mugu-scroll-lines -3) "scroll up" :color red)
  ("x" counsel-M-x "execute" :column "5-Execute")
  ("r" ivy-resume "resume last interactive session")
  (":" eval-expression "eval expression")
  ("SPC" mugu-menu-call-mode-menu "major mode" :column "6-Submenu")
  ("w" mugu-workspace-hydra-menu/body "workspace")
  ("p" mugu-project-menu "project")
  ("h" mugu-menu-help-hydra/body "help")
  ("!" mugu-lint-menu/body "linting")
  ("o" mugu-orgi-menu-global "orgmode")
  ("z" mugu-window-menu "window")
  ("q" nil "cancel hydra" :color blue :column nil))

(defhydra mugu-menu-help-hydra (:color blue
                                       :hint nil)
  "
                                -- HELP MENU --

"
  ("h" help "general help" :column "1-Describe")
  ("s" counsel-info-lookup-symbol "symbol" :column "1-Describe")
  ("f" counsel-describe-function "function")
  ("v" counsel-describe-variable "variable")
  ("b" counsel-descbinds "binding")
  ("d" counsel-describe-face "face")
  ("aa" apropos "symbols" :column "2-Apropos")
  ("ac" apropos-command "commands")
  ("av" apropos-variable "variables")
  ("ar" apropos-value "value")
  ("al" apropos-library "feature")
  ("ad" apropos-documentation "documentation")
  ("q" nil "quit" :column nil))

(defalias 'mugu-menu-main-menu 'mugu-menu-main-hydra/body)
(defalias 'mugu-menu-help-menu 'mugu-menu-help-hydra/body)

(provide 'mugu-menu)
;;; mugu-menu ends here
