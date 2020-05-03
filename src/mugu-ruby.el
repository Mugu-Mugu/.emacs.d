;;; mugu-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-lang)
(require 'rspec-mode)

(defvar-local mugu-ruby-prettify-cmd "rubocop -x" "Base command to prettify a file.")

(defun mugu-ruby-toggle-spec-and-target ()
  "Try to toggle method spec and implem."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (ignore-errors (rspec-toggle-spec-and-target-find-example))
    (when (eq buffer-name (buffer-name))
      (rspec-toggle-spec-and-target))))

(defun mugu-ruby-prettify (file)
  "Prettify FILE."
  (interactive (list buffer-file-truename))
  (if (eq major-mode 'ruby-mode)
      (progn
        (shell-command (format "%s %s" mugu-ruby-prettify-cmd file))
        (revert-buffer 'ignore-auto 'no-confirm))
    (message "Can only prettify ruby file")))

(defun mugu-ruby-verify-method ()
  "Try to verify current method."
  (interactive)
  (if (rspec-spec-file-p (buffer-file-name))
      (ignore-errors (save-excursion (rspec-toggle-spec-and-target-find-example)
                                     (rspec-verify-method)
                                     (rspec-toggle-spec-and-target-find-example)))
    (rspec-verify-method)))

(mugu-define-lang-mode "ruby" "")

(provide 'mugu-ruby)
;;; mugu-ruby ends here
