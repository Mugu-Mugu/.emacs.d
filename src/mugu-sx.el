;;; mugu-sx --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'general)
(require 'sx)
(require 'sx-question)
(require 'sx-interaction)
(require 'sx-search)
(require 'evil)
(require 'mugu-menu)

(defmenu mugu-sx-menu-questions
 (:color amaranth :hint nil)
 "bindings for stackoverflow"
 ("j" sx-question-mode-next-section "↓ section" :column "Navigate")
 ("k" sx-question-mode-previous-section "↑ section")
 ("o" sx-question-mode-order-by "order by?" :column "Misc")
 ("q" nil "quit" :color blue :column nil))

(defmenu mugu-sx-menu-questions-list
 (:color amaranth :hint nil)
 "bindings for stackoverflow"
 ("j" sx-question-list-next "↓ question" :column "Navigate")
 ("k" sx-question-list-previous "↑ question")
 ("v" sx-display "view" :column "View" :color blue)
 ("RET" sx-visit-externally "view external")
 ("h" sx-question-list-hide "hide")
 ("o" sx-question-list-order-by "order questions" :column "Other")
 ("r" sx-question-list-refresh "refresh questions")
 ("s" sx-search "search")
 ("q" (quit-window) "quit" :color blue :column nil))

(defun mugu-sx-quit-issue-view ()
  "Quit the current sx question view and reactivate sx question list menu if needed."
  (interactive)
  (when (eq major-mode 'sx-question-mode)
    (quit-window))
  (when (eq major-mode 'sx-question-list-mode)
    (mugu-sx-menu-questions-list)))

(defun mugu-sx-menu-questions-list-delayed ()
  "Display the menu after a short delay.
Break sx otherwise..."
  (run-with-timer 0.3 nil 'mugu-sx-menu-questions-list))

(defun mugu-sx-configure-bindings ()
  "Configure sx mode bindings."
  (general-def 'motion sx-question-mode-map
    "q" 'mugu-sx-quit-issue-view)
  (evil-set-initial-state 'sx-question-list-mode 'motion)
  (evil-set-initial-state 'sx-question-mode 'motion)
  (add-hook 'sx-question-mode-hook 'mugu-sx-menu-questions)
  (mugu-menu-register-mode-menu 'sx-question-list-mode 'mugu-sx-menu-questions-list)
  (mugu-menu-register-mode-menu 'sx-question-mode 'mugu-sx-menu-questions))

(provide 'mugu-sx)
;;; mugu-sx ends here
