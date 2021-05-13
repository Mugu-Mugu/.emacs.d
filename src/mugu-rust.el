;;; mugu-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-misc)
(require 'mugu-lang)
(require 'rustic)
(require 'rustic-doc)

(defalias 'mugu-rust-doc-dir #'rustic-doc--project-doc-dest)

(eval-when-compile
  (defun mugu-rust-doc-toc ()
    "Search for a rust doc file"
    (interactive)
    (require 'mugu-counsel)
    (mugu-counsel-fzf-file (mugu-rust-doc-dir)))

  (defun mugu-rust-doc-search ()
    "Replacement for `rustic-doc-search-function'.
DIRECTORY and SEARCH have same behavior."
    (interactive)
    (require 'counsel)
    (let ((counsel-rg-base-command (concat (rustic-doc-default-rg-search-command) " %s")))
      (counsel-rg "" (mugu-rust-doc-dir)))))

(mugu-define-lang-mode "rust" "")

(provide 'mugu-rust)
;;; mugu-rust ends here
