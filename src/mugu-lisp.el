;;; Package --- Summary
;; TBC
;;; Commentary:
(require 'mugu-menu)
(require 'elisp-slime-nav)
(require 'lispyville)
(require 'mugu-lang)

;;; Code:
(defun mugu-lisp-insert-header-footer ()
  "Insert default header and footer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert (format ";;; %s --- #{Summary} -*- lexical-binding: t -*-\n" (file-name-base buffer-file-name)))
    (insert ";;; Commentary:\n")
    (insert "\n;;; Code:\n")
    (goto-char (point-max))
    (insert (format "\n(provide '%s)" (file-name-base buffer-file-name)))
    (insert (format "\n;;; %s ends here" (file-name-base buffer-file-name)))
    (save-buffer)))

(defun mugu-lisp-jump-symbol-at-point ()
  "Go to definition of symbol at point."
  (interactive)
  (let ((sym (read (thing-at-point 'symbol t))))
    (cond ((fboundp sym) (find-function `,sym))
          ((boundp sym) (find-variable sym))
          (t (message "%s not defined either as a var or a function" sym)))))

(defun mugu-lisp-prettify ()
  "Jump to definition of thing at point."
  (interactive)
  (if (eq 'emacs-lisp-mode major-mode)
      (lispyville-prettify (point-min) (point-max))
    (message "not a emacs lisp buffer")))

(defun mugu-lisp--insert-package-prefix ()
  "Insert package prefix at point."
  (interactive)
  (insert (format " %s-" (file-name-base buffer-file-name)))
  (when (fboundp 'evil-insert) (evil-insert 1)))

(defmenu mugu-lisp-additional-menu
  (:color blue :hint nil)
  "Lisp additional menu"
  ("eb" eval-buffer "eval buffer" :column "1-Eval")
  ("es" eval-last-sexp "eval sexp")
  ("d" mugu-lisp-insert-header-footer "insert header/footer docstring" :column "2-Misc")
  ("p" mugu-lisp--insert-package-prefix "insert package prefix")
  ("h" elisp-slime-nav-describe-elisp-thing-at-point "help at point"))

(defalias 'mugu-lisp-lang-menu #'mugu-lang-menu)

(mugu-define-lang-mode elisp "elisp mode")

(provide 'mugu-lisp)
;;; mugu-lisp ends here
