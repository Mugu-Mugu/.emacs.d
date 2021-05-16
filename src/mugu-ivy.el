;;; mugu-ivy --- Summary
;; tbc
;;; Commentary:

;;; Code:
(require 'ivy)
(require 'mugu-menu)
(require 'mugu-window)

(defun mugu-ivy-yank-action (x)
  "Yank the candidate X."
  (kill-new x))

(defun mugu-ivy-copy-to-buffer-action (x)
  "Insert the candidate X in the current buffer."
  (with-ivy-window
    (if (file-exists-p x)
          (insert (expand-file-name x))
        (insert x))))

(defun mugu-ivy-install-new-actions ()
  "Add new universal actions."
  (ivy-set-actions
   t
   '(("p" mugu-ivy-copy-to-buffer-action "insert")
     ("y" mugu-ivy-yank-action "yank"))))

(defun mugu-ivy-set-config ()
  "Gather all ivy configuration."
  (mugu-window-configure-side-window "\\*ivy-occur*" 'bottom 0.8)
  (setq ivy-read-action-function 'ivy-read-action-by-key)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 20
        ivy-wrap t
        ivy-preferred-re-builders '((ivy--regex-ignore-order . "order")
                                    (ivy--regex-plus . "ivy")
                                    (ivy--regex-fuzzy . "fuzzy"))
        ivy-initial-inputs-alist '((org-refile . "^ ")
                                   (org-agenda-refile . "^ ")
                                   (org-capture-refile . "^ ")
                                   (counsel-M-x . "^ ")
                                   (counsel-describe-function . "^ ")
                                   (counsel-describe-variable . "^ ")
                                   (counsel-org-capture . "^ ")
                                   (Man-completion-table . "^ ")
                                   (woman . "^ "))))

(defun mugu-ivy-install-keybinds ()
  "Install the keybinds for ivy sessions."
  (setq ivy-hooks-alist '((t . mugu-ivy-passive-menu)))
  (general-define-key :keymaps 'ivy-minibuffer-map
                      "j" (general-key-dispatch 'self-insert-command
                            :timeout 0.2
                            "k" #'mugu-ivy-active-menu)))

(defmenu mugu-ivy-passive-menu (:hint nil :color pink)
  ("M-h" ivy-previous-history-element "prev" :column "History")
  ("M-l" ivy-next-history-element "next")
  ("C-r" ivy-reverse-i-search "reverse search")
  ("M-f" ivy-avy "fast select" :column "Candidates" :color blue)
  ("M-i" ivy-insert-current "insert current")
  ("M-j" ivy-next-line "↓ next")
  ("M-k" ivy-previous-line "↑ prev")
  ("C-w" backward-kill-word "")
  ("M-r" ivy-restrict-to-matches "restrict" :column "Matches")
  ("M-y" ivy-kill-ring-save "yank all")
  ("C-o" mugu-ivy-active-menu "open hydra" :color blue)
  ("RET" ivy-done "done with default" :color blue :column "Done")
  ("M-<return>" ivy-immediate-done "done with current" :color blue :column "Done")
  ("M-o" ivy-dispatching-done "done and select" :color blue)
  ("<escape>" keyboard-escape-quit "exit" :color blue)
  ("M-m" ivy-mark "mark candidate" :column "Mark")
  ("M-u" ivy-unmark "unmark candidate")
  ("C-g" keyboard-escape-quit "exit" :color blue))

(defmenu mugu-ivy-active-menu (:hint nil :color amaranth :inherit (mugu-ivy-passive-menu-hydra/heads))
  "
Selected action : %s(ivy-action-name) %s(if ivy-calling \"auto called\" \"\")
"
  ("j" ivy-next-line "by candidate" :column "Navigate ↓")
  ("l" ivy-scroll-up-command "by page")
  ("G" ivy-end-of-buffer "to bottom")
  ("k" ivy-previous-line "by candidate" :column "Navigate ↑")
  ("h" ivy-scroll-down-command "by page")
  ("gg" ivy-beginning-of-buffer "to top")

  ("b" backward-word)
  ("w" forward-word)

  ("m" ivy-mark "mark candidate" :column "Mark")
  ("u" ivy-unmark "unmark candidate")
  ("DEL" ivy-unmark-backward "undo last mark")

  ;; actions
  ("q" keyboard-escape-quit "exit" :exit t :column nil)
  ("C-g" keyboard-escape-quit "exit" :exit t)
  ("<escape>" keyboard-escape-quit "exit" :exit t)
  ("i" mugu-ivy-passive-menu "insert" :color blue)
  ("C-o" mugu-ivy-passive-menu "insert" :color blue)

  ("f" ivy-call "call once" :column "Call")
  ("c" ivy-toggle-calling "toogle call")
  ("a" ivy-read-action "select call")

  ;; ("d" ivy-done :exit t)
  ("RET" ivy-done "done default" :color blue :column "Done")
  ("C-j" ivy-alt-done "take input" :color blue)
  ("TAB" ivy-dispatching-done "done choose" :color blue)
  ("M-o" ivy-dispatching-done "done choose" :color blue)
  ("o" ivy-occur "make occur" :exit t))

(provide 'mugu-ivy)
;;; mugu-ivy ends here
