;;; mugu-yasnippet --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
(require 'mugu-menu)
(require 'yasnippet)

;;; Code:
(defmenu mugu-yasnippet-menu (:color blue :hint nil)
  "Yasnippet menu"
  ("i" mugu-yasnippet-insert "yas-insert-snippet" :column "actions")
  ("e" yas-expand-snippet "yas-expand-snippet")
  ("n" yas-new-snippet "yas-new-snippet" :column "define new")
  ("t" yas-tryout-snippet "yas-tryout-snippet")
  ("ae" yas-activate-extra-mode "yas-activate-extra-mode" :column "misc")
  ("d" yas-describe-tables "yas-describe-tables")
  ("g" yas-visit-snippet-file "yas-visit-snippet-file")
  ("ld" yas-load-directory "yas-load-directory" :column "load things")
  ("lb" yas-load-snippet-buffer "yas-load-snippet-buffer"))


;;;###autoload
(defun mugu-yasnippet-insert ()
  "Insert a snippet but go in insert state if needed."
  (interactive)
  (when (evil-normal-state-p) (evil-insert-state))
  (yas-insert-snippet))

(provide 'mugu-yasnippet)
;;; mugu-yasnippet ends here
