;;; mugu-roam --- #{Summary}
;;; Commentary:

(require 'org-roam)
(require 'f)

(defun mugu-roam-insert ()
  "A simple wrapper to allow to chain insert from normal state."
  (interactive)
  (org-roam-node-insert nil)
  (insert " "))

(defun mugu-roam-daily-filename ()
  "A simple wrapper to go to daily note file without capture org shenanigans.
A bit hacky but the roam interface is kinda bad."
  (interactive)
  (f-join org-roam-dailies-directory (format "%s.org" (format-time-string "%Y-%m-%d"))))

(defun mugu-roam-capture-chore ()
  "."
  (interactive)
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-capture-templates `(("x" "capture a task todo"
                                    checkitem (file+headline ,(mugu-roam-daily-filename) "Chores") nil))))
      (org-capture nil "x")
      (evil-insert-state))))

;;;###autoload
(defun mugu-roam-capture-daily-todo ()
  "Capture TODO to daily note."
  (interactive)
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-roam-dailies-capture-templates `(("x" "capture a task todo"
                                                 entry ,(format "* %s %%?\n%%i" (format-time-string "%H:%M"))
                                                 :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
                                                 :unnarrowed t))))
      (org-capture nil "x")
      (evil-insert-state))))

;;;###autoload
(defun mugu-roam-capture-daily-note ()
  "Capture TODO to daily note."
  (interactive)
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-roam-dailies-capture-templates `(("x" "capture a task todo"
                                    entry ,(format "* %s %%?\n%%i" (format-time-string "%H:%M"))
                                    :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
                                    :unnarrowed t))))
      (org-capture nil "x")
      (evil-insert-state))))

;;;###autoload
(defun mugu-roam-capture-daily-todo-with-link ()
  "Capture a todo and store it in the given HEADLINE as child.
The hack with noflet is to prevent fucking orgmode to sabotage the windows configuration."
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-capture-templates `(("x" "capture a task todo"
                                    entry (file ,(mugu-roam-daily-filename)) "* TODO %?\n%l\n%i"
                                    :unnarrowed t))))
      (org-capture nil "x"))))

;;; Code:

(provide 'mugu-roam)
;;; mugu-roam ends her
