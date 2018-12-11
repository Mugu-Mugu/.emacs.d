;;; Package --- Summary
;; tbc
;;; Commentary:
;;; Code:

;; fixme core function should be made available elsewhere
(require 'mugu-core)

(use-package mugu-org-menu
  :commands mugu-org-menu/main-menu
  :functions mugu-menu-register-mode-menu
  :requires (mugu-menu)
  ;; autoload itself after org is loaded (no way to do it natively with usepackage)
  :init (after 'org (require 'mugu-org-menu))
  ;; after loading, ensure org is present
  :config (require 'org)
  ;; seems complex but needed to ensure everything work whether the menu or a orgfile is accessed first
  ;; plus mugu-org-menu is an addon to org configuration so it was not desirable to have it required by
  ;; it
  )

(use-package org-agenda
  :defer
  :functions mugu-org-menu/agenda-menu mugu-menu-register-mode-menu
  :after org
  :config
  (after 'mugu-org-menu
    (add-hook 'org-agenda-mode-hook #'mugu-org-menu/agenda-menu)
    (mugu-menu-register-mode-menu 'org-agenda-mode 'mugu-org-menu/agenda-menu))
  :bind
  (:map org-agenda-mode-map
        ("SPC" . mugu-menu-main-menu)))

(use-package org
  :ensure
  :defer t
  :functions mugu-org-menu/add-head-to-main org-eval-in-calendar mugu-org-workflow/activate
  :defines org-agenda-custom-commands org-capture-templates mugu-org-menu/add-head-to-main
  :config
  (require 'org)
  (require 'org-agenda)
  (require 'mugu-org-utils)
  (require 'mugu-org-workflow)

  (setq org-use-fast-todo-selection t)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-indented t)
  (setq org-lowest-priority ?F)

  ;; hjkl binding for quick date selection
  (define-key org-read-date-minibuffer-local-map (kbd "M-l")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-h")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))

  ;; Ugly hack
  ;; redefine because org-no-popup actually do the opposite of what was intended
  (defun org-switch-to-buffer-other-window (&rest args)
    "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
    (apply 'switch-to-buffer-other-window args))

  ;; open at bottom of screen
  (add-to-list 'display-buffer-alist
               '(" \\*Org todo\\*"
                 (display-buffer-in-side-window)
                 (window-height . 1)
                 (inhibit-switch-frame . t)
                 (inhibit-same-window . t)))
  (add-to-list 'display-buffer-alist
               '("\\*Org Agenda\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 0.5)
                 (inhibit-switch-frame . t)
                 (inhibit-same-window . t)))

  (setq org-agenda-files `(,(expand-file-name "~/org")
                           ,(expand-file-name (concat user-emacs-directory "emacs.org"))))

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit 't)

  ;; speedup
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category))

  ;;Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

  (mugu-org-workflow/activate)

  (after 'mugu-org-menu
    (mugu-menu-register-mode-menu 'org-mode 'mugu-org-menu/org-menu)
    (mugu-org-menu/add-head-to-main '("g" (switch-to-buffer (mugu-orgu/get-last-buffer-name)) "go to last org buffer" :color blue))
    (mugu-org-menu/add-head-to-main '("r" mugu-org-workflow/refile-task "refile tasks" :color blue))
    (mugu-org-menu/add-head-to-main '("p" mugu-org-workflow/goto-progress-task "goto in progress task" :color blue))))

;;; mugu-org ends here
(provide 'mugu-org)
