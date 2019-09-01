;;; mugu-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-window-utils)
(require 'mugu-lang)
(require 'rspec-mode)

(defvar mugu-ruby-prettify-cmd "rubocop -x" "Base command to prettify a file.")

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
      (shell-command (format "%s %s" mugu-ruby-prettify-cmd file))
    (message "Can only prettify ruby file")))

(defun mugu-ruby-verify-method ()
  "Try to verify current method."
  (interactive)
  (if (rspec-spec-file-p (buffer-file-name))
      (ignore-errors (save-excursion (rspec-toggle-spec-and-target-find-example)
                                     (rspec-verify-method)
                                     (rspec-toggle-spec-and-target-find-example)))
    (rspec-verify-method)))

(defun mugu-ruby-activate ()
  "Activate ruby configuration."
  (mugu-window-configure-side-window "\\*rspec-compilation\\*" 'top 0.7)
  (mugu-ruby-activate-rspec-binding))

(defalias 'mugu-ruby-lang-menu #'mugu-lang-menu)

(defun mugu-ruby-activate-rspec-binding ()
  "."
  (general-define-key :keymaps 'ruby-mode-map
                      [remap mugu-lang-format-buffer] #'mugu-ruby-prettify
                      [remap mugu-lang-test-file] #'rspec-verify
                      [remap mugu-lang-test-rerun-last] #'rspec-rerun
                      [remap mugu-lang-test-toggle-goto] #'mugu-ruby-toggle-spec-and-target
                      [remap mugu-lang-test-method] #'mugu-ruby-verify-method
                      [remap mugu-lang-test-single-at-point] #'rspec-verify-single
                      [remap mugu-lang-test-all-in-project] #'rspec-verify-all))

(provide 'mugu-ruby)
;;; mugu-ruby ends here
