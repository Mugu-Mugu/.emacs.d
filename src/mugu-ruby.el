;;; mugu-ruby --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'mugu-lsp)
(require 'mugu-menu)

;; * begin:
 (defmenu mugu-ruby-menu
  (:color blue :hint nil :inherit (mugu-lsp-menu-hydra/heads))
  "menu for ruby mode"
  ("tt" rspec-verify "verify test" :column "rspec")
  ("tr" rspec-rerun "rerun test")
  ("tg" mugu-ruby-toggle-spec-and-target "switch spec/file")
  ("tm" mugu-ruby-verify-method "verify method")
  ("ts" rspec-verify-single "verify example")
  ("ta" rspec-verify-all "verify project"))

(defun mugu-ruby-toggle-spec-and-target ()
  "Try to toggle method spec and implem."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (ignore-errors (rspec-toggle-spec-and-target-find-example))
    (when (eq buffer-name (buffer-name))
      (rspec-toggle-spec-and-target))))

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
  (mugu-menu-register-mode-menu 'ruby-mode 'mugu-ruby-menu))

(provide 'mugu-ruby)
;;; mugu-ruby ends here
