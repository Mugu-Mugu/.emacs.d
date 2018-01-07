;;; Package --- Summary
;; tbc
;;; Commentary:
;;; Code:

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
  :functions mugu-org-menu/add-head-to-main
  :defines org-agenda-custom-commands org-capture-templates mugu-org-menu/add-head-to-main
  :config
  (require 'org-agenda)
  (require 'mugu-org-utils)

  (setq org-todo-keywords (quote ((sequence "SOMEDAY(s)" "TODO(t)" "REVIEW(r)" "NEXT(n)"
                                            "IN_PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")
                                  (sequence "FAST_TODO(f)" "|" "DONE(d)"))))
  (setq org-use-fast-todo-selection t)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-indented t)

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

  (defun mugu-org/make-capture-binding ()
    "Build capture bindings for all registered org-file.
Bindings are done if global property MUGU-LABEL is present.
4 bindings are performed : capture immediate, regular, quick todo and journal note."
    (let ((base-capture-template '(("t" "Capture a TODO entry")
                                   ("p" "Capture a IN_PROGRESS entry")
                                   ("f" "Capture a FAST_TODO entry")
                                   ("j" "Capture a note")))
          (func-cap-cmd-< (lambda (it other)
                            "comparator based on binding of a capture cmd"
                            (string< (-first-item it) (-first-item other))))
          (func-gen-todo (lambda (org-file todo-state todo-hotkey)
                           "generate a todo capture spec"
                           `(,(format "%s%s" todo-hotkey (mugu-org-utils/get-file-hotkey org-file))
                             ,(format "Capture a %s entry for %s" todo-state (file-name-base org-file))
                             entry
                             (file+headline ,org-file "Inbox")
                             ,(format "* %s %%i %%?" todo-state))))
          (func-gen-note (lambda (org-file)
                           "generate a note capture spec"
                           `(,(concat "j" (mugu-org-utils/get-file-hotkey org-file))
                             ,(concat "Capture note for " (file-name-sans-extension org-file))
                             entry
                             (file+datetree ,org-file)
                             "* %U %i %?"))))
      (-sort func-cap-cmd-<
             (apply 'append
                    base-capture-template
                    (-map (lambda (org-file)
                            (when (mugu-org-utils/get-file-hotkey org-file)
                              `(,(funcall func-gen-todo org-file "IN_PROGRESS" "p")
                                ,(funcall func-gen-todo org-file "FAST_TODO" "f")
                                ,(funcall func-gen-todo org-file "TODO" "t")
                                ,(funcall func-gen-note org-file))))
                          (org-agenda-files))))))

  (setq org-capture-templates (mugu-org/make-capture-binding))

  (defun mugu-org/active-parent-with-active-childs? ()
    "Return t if the current entry is active and at at least 1 active child."
    (and (org-entry-is-todo-p)
         (string= "IN_PROGRESS" (org-get-todo-state))
         (> (length (org-map-entries nil "/IN_PROGRESS" 'tree)) 1)))

  (defun mugu-org/skip-active-parent-with-active-child ()
    "A skip function for org agenda search based on `mugu-org/active-parent-with-active-childs?'"
    (when (mugu-org/active-parent-with-active-childs?)
      (save-excursion (outline-next-heading))))

  (defun mugu-org/skip-inbox-headline ()
    "A skip function for org agenda to ignore the inbox headline container."
    (when (string= "Inbox" (-fifth-item (org-heading-components)))
      (save-excursion (outline-next-heading))))

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit 't)


  (defun mugu-org/make-project-overview-cmd (binding filename)
    "Make a org agenda custom command entry for on BINDING for FILENAME.
This will define a standard block agenda under the prefix p"
    (let ((the-file (file-name-base filename)))
      `(,(format "p%s" binding)
        ,(format "an overview for project %s" the-file)
        ((todo "IN_PROGRESS"
               ((org-agenda-overriding-header ,(format "Task in progress for project %s" the-file))
                (org-agenda-prefix-format " %b")
                (org-agenda-skip-function #'mugu-org/skip-active-parent-with-active-child)))
         (tags "REFILE"
               ((org-agenda-overriding-header ,(format "Task to refile for project %s" the-file))
                (org-agenda-prefix-format " %b")
                (org-agenda-skip-function #'mugu-org/skip-inbox-headline)))
         (agenda ""
                 ((org-agenda-overriding-header ,(format "Agenda for project %s" the-file))
                  (org-agenda-prefix-format " %s %b")
                  (org-agenda-show-all-dates nil)
                  (org-agenda-ndays 21)))
         (todo "TODO|REVIEW|NEXT|SOMEDAY"
               ((org-agenda-overriding-header ,(format "Backlog of the project %s" the-file))
                (org-agenda-prefix-format " %b")
                (org-agenda-sorting-strategy '(todo-state-down priority-down)))))
        ((org-agenda-files '(,filename))))))


  (setq org-agenda-custom-commands (--map (mugu-org/make-project-overview-cmd (mugu-org-utils/get-file-hotkey it) it) (org-agenda-files)))


  (defun mugu-org/goto-progress-task ()
    "Goto any headline with PROGRESS status."
    (interactive)
    (mugu-org-utils/query-entries #'mugu-org-utils/goto-headline
                                  "/IN_PROGRESS"
                                  'agenda
                                  #'mugu-org/skip-active-parent-with-active-child))

  (defun mugu-org/refile-task ()
    "Goto any headline with REFILE tag."
    (interactive)
    (mugu-org-utils/query-entries #'mugu-org-utils/refile-headline "REFILE/TODO=*"))

  (after 'mugu-org-menu
    (mugu-menu-register-mode-menu 'org-mode 'mugu-org-menu/org-menu)
    (mugu-org-menu/add-head-to-main '("r" mugu-org/refile-task "refile tasks" :color blue))
    (mugu-org-menu/add-head-to-main '("p" mugu-org/goto-progress-task "goto in progress task" :color blue))))

;;; mugu-org ends here
(provide 'mugu-org)
