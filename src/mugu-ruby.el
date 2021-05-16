;;; mugu-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-lang)
(require 'rspec-mode)
(require 'f)

(defvar-local mugu-ruby-prettify-cmd "rubocop -x" "Base command to prettify a file.")
(defvar mugu-ruby-use-robe nil "Indicate if robe should be used for ruby mode.")

(defun mugu-ruby-toggle-spec-and-target ()
  "Try to toggle method spec and implem."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (ignore-errors (rspec-toggle-spec-and-target-find-example))
    (when (eq buffer-name (buffer-name))
      (rspec-toggle-spec-and-target))))

(defun mugu-ruby-prettify (file)
  "Prettify FILE."
  (interactive (list buffer-file-name))
  (if (eq major-mode 'ruby-mode)
      (progn
        (shell-command (format "%s %s" mugu-ruby-prettify-cmd
                               (rspec--shell-quote-local file)))
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

(defun mugu-ruby-rspec-verify-directory ()
  "Verify current directory.  Not recursive."
  (interactive)
  (rspec-compile (-filter #'rspec-spec-file-p (f-files default-directory))
                 (rspec-core-options)))

(defun mugu-ruby-robe-mode-maybe ()
  "Start robe if activated."
  (when mugu-ruby-use-robe
    (eval-when-compile
      (require 'robe)
      (robe-mode))))

(mugu-define-lang-mode "ruby" "")

(provide 'mugu-ruby)
;;; mugu-ruby ends here
