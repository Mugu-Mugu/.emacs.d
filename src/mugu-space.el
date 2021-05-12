;;; mugu-space --- Summary
;; Binding related to the space key
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'helpful)
(require 'general)
(require 'mugu-feature)

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
    ("b" switch-to-buffer "previous buffer" :column "Change File")
    ("ff" (with-mugu-dir (counsel-find-file)) "find files")
    ("fr" (mugu-counsel-fzf-file mugu-directory) "find files recursively")
    ("fa" (mugu-counsel-fzf-any mugu-directory) "find any recursively")
    ("fl" counsel-recentf "file recently used")
    ("t" mugu-vterm-switch "switch vterm" :column "Terminal")
    ("Tc" mugu-vterm-create "create vterm")
    ("Tk" mugu-vterm-kill "kill vterm")
    ("Tr" mugu-vterm-rename "rename vterm")
    ("d" mugu-directory-with-current-file-path "cd to current file" :color red :column "hange Dir")
    ("cd" mugu-counsel-cd "change dir" :color red)
    ("cr" (mugu-counsel-fzf-dir mugu-directory) "change dir recursively" :color red)
    ("ss" counsel-grep-or-swiper "swiper" :column "earch")
    ("sr" (counsel-rg "" mugu-directory) "rgrep")
    ("sx" sx-search "stack exchange")
    ("sgg" mugu-feature-search-google "seach w/ google")
    ("sgt" mugu-feature-search-google-at-point "seach w/ google at point")
    ("y" counsel-yank-pop "yank ring" :column "Misc")
    ("u" counsel-unicode-char "insert unicode")
    ("j" (scroll-up 3) "scroll up" :color red)
    ("k" (scroll-down 3) "scroll down" :color red)
    ("l" ace-link "link" :color blue)
    ("x" counsel-M-x "execute" :column "Execute")
    ("r" ivy-resume "ivy resume")
    ("ee" mugu-eval-and-replace "eval and replace selection")
    (":" eval-expression "eval expression")
    ("SPC" mugu-menu-call-mode-menu "major mode" :column "Submenu")
    ("M-SPC" mugu-menu-call-mode-menu nil)
    ("C-SPC" mugu-menu-call-mode-menu nil)
    ("p" mugu-project-menu "project")
    ("h" mugu-space-help-menu "help")
    (";" mugu-flyspell-menu/body "spelling")
    ("!" mugu-flycheck-menu "linting")
    ("o" mugu-orgi-menu-global "orgmode")
    ("z" mugu-window-menu/body "window")
    ("w" mugu-tab-menu/body "workspace")
    ("vh" mugu-git-tm-menu-or-activate "vc file history")
    ("vv" mugu-git-gutter-menu "vc gutter")
    ("vm" git-messenger:popup-message "vc describe thing")
    ("q" nil "cancel hydra" :color blue :column nil))

  (defmenu mugu-space-help (:color blue
                                   :hint nil)
    "
                                -- HELP MENU --

"
    ("h" helpful-at-point "thing at point" :column "1-Describe")
    ("s" counsel-info-lookup-symbol "symbol" :column "1-Describe")
    ("f" counsel-describe-function "function")
    ("v" counsel-describe-variable "variable")
    ("b" counsel-descbinds "binding")
    ("d" counsel-describe-face "face")
    ("c" mugu-counsel-describe-custom "custom")
    ("ii" Info-goto-emacs-command-node "look for some node" :column "2-Info")
    ("is" counsel-info-lookup-symbol "look for some symbol")
    ("ia" Info-apropos "text based search" )
    ("aa" apropos "symbols" :column "3-Apropos")
    ("ac" apropos-command "commands")
    ("av" apropos-variable "variables")
    ("ar" apropos-value "value")
    ("al" apropos-library "feature")
    ("ad" apropos-documentation "documentation")
    ("q" nil "quit" :column nil))

 (general-def '(motion emacs) "SPC" #'mugu-space-main-menu)
 (general-def '(insert visual) "M-SPC" #'mugu-space-main-menu)
 (general-def '(motion emacs) "M-SPC" (lambda () (interactive)
                                        (forward-char)
                                        (insert " ")
                                        (backward-char))))

(provide 'mugu-space)
;;; mugu-space ends here
