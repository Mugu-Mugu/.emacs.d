;;; Package --- Summary
;; TBC
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'mugu-menu)
(require 'mugu-core)
(require 'mugu-shell-utils)
(require 'mugu-counsel)

(defhydra mugu-menu-shell-main
  (:color blue :hint nil :body-pre (lambda ()
                                     (shell-resync-dirs)
                                     (mugu-shell-scroll-before-insert)))
  "
                                -- SHELL MENU --
  -> Current Dir : %s(mugu-directory-pwd-file)
"
  ("cd" (mugu-shell-change-directory 'read-directory-name "directory:") "find dir" :column "1-find")
  ("cr" (mugu-shell-change-directory 'mugu-counsel-read-recursive-dir) "find dir recursively")
  ("cm" (mugu-shell-change-directory 'mugu-counsel-read-bookmark-dir) "find dir from bookmark")
  ("md" (mugu-shell-change-directory 'mugu-counsel-read-bookmark-dir) "find dir from bookmark")
  ("ff" (insert (read-file-name "select file:  ")) "find file")
  ("fr" (insert (mugu-counsel-read-recursive-file)) "find file recursively")
  ("t" comint-truncate-buffer "trucate shell" :column "2-command")
  ("a" counsel-shell-history "again command")
  ("C-r" counsel-shell-history "again command"))

(provide 'mugu-shell-menu)
;;; mugu-shell-menu ends here
