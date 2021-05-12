;;; mugu-vterm-snippets --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

(require 'vterm)
(require 'mugu-vterm)
(require 'yasnippet)
(require 'mugu-project)

;;; Code:
(defun mugu-vterm-snippet-condition-project-p (project-name)
  "Predicate enabling snippet if current project name is PROJECT-NAME."
   (message "mugu %s" project-name)
   (equal project-name (mugu-project-current-project-name)))

(defun mugu-vterm-snippet--before-expansion ()
  "."
  (read-only-mode -1)
  (vterm-copy-mode))

(defun mugu-vterm-snippet--after-expansion ()
  "."
  (let ((expanded-template (buffer-substring yas-snippet-beg yas-snippet-end)))
    (vterm-copy-mode -1)
    (read-only-mode 1)
    (vterm-insert expanded-template)))


(defun mugu-vterm-snippet--install (&optional buffer)
  "Install vterm snippet scaffolding on BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local yas-buffer-local-condition (cons 'require-snippet-condition t))
    (add-hook 'yas-before-expand-snippet-hook #'mugu-vterm-snippet--before-expansion 0 'local)
    (add-hook 'yas-after-exit-snippet-hook #'mugu-vterm-snippet--after-expansion 0 'local)))

(defun mugu-vterm-snippet--uninstall (&optional buffer)
  "Uninstall vterm snippet scaffolding on BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local yas-buffer-local-condition (get 'default yas-buffer-local-condition))
    (remove-hook 'yas-before-expand-snippet-hook #'mugu-vterm-snippet--before-expansion )
    (remove-hook 'yas-after-exit-snippet-hook #'mugu-vterm-snippet--after-expansion)))

(defun mugu-vterm-snippet--activate ()
  "."
  (-each (mugu-vterm-list-buffer) #'mugu-vterm-snippet--install)
  (add-hook 'vterm-mode-hook #'mugu-vterm-snippet--install))

(defun mugu-vterm-snippet--deactivate ()
  "."
  (-each (mugu-vterm-list-buffer) #'mugu-vterm-snippet--uninstall)
  (remove-hook 'vterm-mode-hook #'mugu-vterm-snippet--install))

(define-minor-mode mugu-vterm-snippet-mode
  "A mode enabling usage of `yasnippet' in `vterm'.
Not that trivial, since vterm has harsh requirements about what can be done in the buffer."
  :global t
  :group 'mugu
  (if mugu-vterm-snippet-mode
      (mugu-vterm-snippet--activate)
    (mugu-vterm-snippet--deactivate)))


(provide 'mugu-vterm-snippet)
;;; mugu-vterm-snippet ends here
