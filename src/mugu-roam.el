;;; mugu-roam --- #{Summary}
;;; Commentary:

(require 'org-roam)
(require 'f)

(defun mugu-roam-insert ()
  "A simple wrapper to allow to chain insert from normal state."
  (interactive)
  (org-roam-insert nil)
  (insert " "))

(defun mugu-roam-daily-filename ()
  "A simple wrapper to go to daily note file without capture org shenanigans.
A bit hacky but the roam interface is kinda bad."
  (interactive)
  (f-join org-roam-directory (format "%s.org" (format-time-string "%Y-%m-%d"))))

;;;###autoload
(defun mugu-roam-capture-daily-todo ()
  "Capture TODO to daily note."
  (interactive)
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-capture-templates `(("x" "capture a task todo"
                                    entry (file ,(mugu-roam-daily-filename)) "* TODO %?\n%i"
                                    :unnarrowed t))))
      (org-capture nil "x")
      (evil-insert-state))))

;;;###autoload
(defun mugu-roam-capture-daily-note ()
  "Capture TODO to daily note."
  (interactive)
  (noflet ((delete-other-windows (&optional _window) (set-window-configuration (org-capture-get :return-to-wconf))))
    (let ((org-capture-templates `(("x" "capture a task todo"
                                    entry (file ,(mugu-roam-daily-filename)) ,(format "* %s %%?\n%%i" (format-time-string "%H:%M"))
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

(setq org-roam-dailies-capture-templates
      '(("d" "daily" plain (function org-roam-capture--get-point)
         ""
         :immediate-finish t
         :file-name "%<%Y-%m-%d>"
         :head "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: fast")))

;;; Code:

(provide 'mugu-roam)
;;; mugu-roam ends her
