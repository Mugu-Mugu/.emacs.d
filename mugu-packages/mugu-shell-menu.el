(require 'hydra)
(require 'mugu-menu)
(require 'mugu-core)
(require 'mugu-shell-utils)



(defhydra mugu-menu-shell-main
  (:color blue :hint nil :body-pre (lambda () (mugu-shell-scroll-before-insert)))
  "
                                -- SHELL MENU --
  -> Current Dir : %s(mugu-directory-pwd)
" 
  ("cd" (lambda () (interactive) (mugu-shell-change-directory 'mugu-shell--find-dir)) "find dir" :column "1-find")
  ("cr" (lambda () (interactive) (mugu-shell-change-directory 'mugu-shell--find-dir-recursive)) "find dir recursively")
  ("cm" (lambda () (interactive) (mugu-shell-change-directory 'mugu-shell--load-bookmark-dir)) "find dir recursively")
  ("f" mugu-shell--find-file "find file")
  ("r" mugu-shell--find-file-recursive "find file recursively")
  ("t" comint-truncate-buffer "trucate shell" :column "2-command")
  ("a" counsel-shell-history "again command")
  ("C-r" counsel-shell-history "again command"))


(provide 'mugu-shell-menu)
