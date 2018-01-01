;;; Package --- Summary
;; TBC
;;; Commentary:

;;; Code:
(defun mugu-lisp-insert-header-footer ()
  "Insert default header and footer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert (format ";;; %s --- Summary\n" (file-name-base)))
    (insert ";; tbc\n")
    (insert ";;; Commentary:\n")
    (insert "\n;;; Code:\n")
    (goto-char (point-max))
    (insert (format "\n(provide '%s)" (file-name-base)))
    (insert (format "\n;;; %s ends here" (file-name-base)))
    (save-buffer)))

(defun mugu-lisp-jump-symbol-at-point ()
  "Go to definition of symbol at point."
  (interactive)
  (let ((sym (read (thing-at-point 'symbol t))))
    (cond ((fboundp sym) (find-function `,sym))
          ((boundp sym) (find-variable sym))
          (t (message "%s not defined either as a var or a function" sym)))))

(defun mugu-lisp--insert-package-prefix ()
  "Insert package prefix at point."
  (interactive)
  (insert (format "'%s-" (file-name-base)))
  (when (fboundp 'evil-insert) (evil-insert)))

(defhydra mugu-lisp-main-hydra
  (:color blue :hint nil)
  "
                                -- LISP MENU --
"
  ("eb" eval-buffer "eval buffer" :column "1-Eval")
  ("es" eval-last-sexp "eval sexp")
  ("d" mugu-lisp-insert-header-footer "insert header/footer docstring" :column "2-Misc")
  ("p" mugu-lisp--insert-package-prefix "insert package prefix")
  ("g" mugu-lisp-jump-symbol-at-point "go to symbol" :column "3-Goto"))

;;;###autoload
(defalias 'mugu-lisp-main-menu 'mugu-lisp-main-hydra/body)

(provide 'mugu-lisp-utils)
;;; mugu-lisp-utils ends here
