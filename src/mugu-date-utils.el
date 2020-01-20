;;; mugu-date-utils --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
;;; small stupid package to fill a hole in org mode: it is really innefficient
;;; to specify time part in the org calendar view

(require 'general)
(require 'org)
(require 'dash)
(require 'mugu-window)

;;; Code:
(defun mugu-date-utils--increment-hours (inc)
  "Increment to current time prompt by INC hours."
  (mugu-date-utils-redisplay-time
   (seconds-to-time (+ (mugu-date-utils--current-time)
                       (* 3600 inc)))))

(defun mugu-date-utils--increment-minutes (inc)
  "Increment to current time prompt by INC minutes."
  (mugu-date-utils-redisplay-time
   (seconds-to-time (+ (mugu-date-utils--current-time)
                       (* 60 inc)))))

(defun mugu-date-utils--reset-time ()
  "Reset the time part of the current date selection."
  (delete-minibuffer-contents))

(defun mugu-date-utils--reset-to-now ()
  "Reset the time to now."
  (mugu-date-utils-redisplay-time (+ (-first-item (current-time-zone)) (float-time))))

(defun mugu-date-utils--reset-minutes ()
  "Reset the minute in the time part of the current date selection."
  (mugu-date-utils-redisplay-time
   (seconds-to-time
    (- (mugu-date-utils--current-time) (% (mugu-date-utils--current-time) 3600)))))

(defun mugu-date-utils--current-time ()
  "Return the current time in the minibuffer or now if absent."
  (let* ((raw-time-from-minibuffer (parse-time-string (minibuffer-contents)))
         (time-from-minibuffer (+ (* 60 (or (-second-item raw-time-from-minibuffer) 0))
                                  (* 3600 (or (-third-item raw-time-from-minibuffer) 0)))))
    (or (and (not (s-blank-str? (minibuffer-contents))) time-from-minibuffer)
        (float-time))))

(defun mugu-date-utils-redisplay-time (new-time)
  "Redisplay the value of NEW-TIME."
  (mugu-date-utils--reset-time)
  (insert (format-time-string "%R" new-time t)))

(defun mugu-date-utils-read-date ()
  "Interactivly pick a date and return it in the form of seconds since epoch."
  (interactive)
  (time-to-seconds (encode-time (-replace nil 0 (parse-time-string (org-read-date))))))

(defun mugu-date-utils-configure ()
  "Setup configuration for better time pick for org mode."
  (mugu-window-configure-side-window "\\*Calendar\\*" 'bottom 0.3)
  (general-def org-read-date-minibuffer-local-map
    "rt" (lambda () (interactive) (mugu-date-utils--reset-time))
    "rm" (lambda () (interactive) (mugu-date-utils--reset-minutes))
    "rr" (lambda () (interactive) (mugu-date-utils--reset-to-now))
    "J" (lambda () (interactive) (mugu-date-utils--increment-minutes 10))
    "K" (lambda () (interactive) (mugu-date-utils--increment-minutes (- 10)))
    "M-k" (lambda () (interactive) (mugu-date-utils--increment-hours 1))
    "M-j" (lambda () (interactive) (mugu-date-utils--increment-hours (- 1)))
    "M-l" (lambda () (interactive) (mugu-date-utils--increment-hours 4))
    "M-h" (lambda () (interactive) (mugu-date-utils--increment-hours (- 4)))
    "l" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
    "h" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
    "j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
    "k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))))

(provide 'mugu-date-utils)
;;; mugu-date-utils ends here
