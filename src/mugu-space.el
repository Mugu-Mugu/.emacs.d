;;; mugu-space --- Summary
;; Binding related to the space key
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'general)

(defun mugu-space-activate-helm-menu ()
  "."
  (error "To be defined?"))

(defun mugu-space-activate-ivy-menu ()
  "."
  (defmenu mugu-space-main (:color blue :hint nil)
    "
                               -- MAIN MENU --

  -> File    Dir : %s(mugu-directory-pwd-file)
  -> Current Dir : %s(mugu-directory-pwd)
"
    ("b" mugu-menu-switch-buffer "previous buffer" :column "1-Change File")
    ("ff" (with-mugu-dir (counsel-find-file)) "find files")
    ("fr" (mugu-counsel-fzf-file mugu-directory) "find files recursively")
    ("fa" (mugu-counsel-fzf-any mugu-directory) "find any recursively")
    ("fl" counsel-recentf "file recently used")
    ("d" mugu-directory-with-current-file-path "cd to current file" :color red :column "2-Change Dir")
    ("cd" mugu-counsel-cd "change dir" :color red)
    ("cr" (mugu-counsel-fzf-dir mugu-directory) "change dir recursively" :color red)
    ("ss" counsel-grep-or-swiper "swiper" :column "3-Search")
    ("sr" (counsel-rg "" mugu-directory) "rgrep")
    ("sx" sx-search "stack exchange")
    ("sgl" google-this-lucky-search "seach w/ google (lucky)")
    ("sgg" google-this "seach w/ google")
    ("y" counsel-yank-pop "yank ring" :column "4-Misc")
    ("u" counsel-unicode-char "insert unicode")
    ("j" (mugu-scroll-lines 3) "scroll down" :color red)
    ("k" (mugu-scroll-lines -3) "scroll up" :color red)
    ("l" ace-link "link" :color blue)
    ("x" counsel-M-x "execute" :column "5-Execute")
    ("r" ivy-resume "ivy resume")
    (":" eval-expression "eval expression")
    ("SPC" mugu-menu-call-mode-menu "major mode" :column "6-Submenu")
    ("p" mugu-project-menu "project")
    ("h" mugu-space-help-menu "help")
    ("!" mugu-flycheck-menu "linting")
    ("o" mugu-orgi-menu-global "orgmode")
    ("z" mugu-window-menu "window")
    ("vh" mugu-git-tm-menu-or-activate "vc file history")
    ("vm" git-messenger:popup-message "vc describe thing")
    ("q" nil "cancel hydra" :color blue :column nil))

  (defmenu mugu-space-help (:color blue
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
    ("c" mugu-counsel-describe-custom "custom")
    ("aa" apropos "symbols" :column "2-Apropos")
    ("ac" apropos-command "commands")
    ("av" apropos-variable "variables")
    ("ar" apropos-value "value")
    ("al" apropos-library "feature")
    ("ad" apropos-documentation "documentation")
    ("q" nil "quit" :column nil))

  (general-def '(motion emacs) "SPC" #'mugu-space-main-menu))

(provide 'mugu-space)
;;; mugu-space ends here
