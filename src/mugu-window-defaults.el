;;; mugu-window-defaults --- Define sane defaults for window -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun mugu-window-defaults-activate ()
  "Define various sane defaults for window management/display."
  (winner-mode 1)
  ;; configure boring buffer disposition
  (add-to-list 'display-buffer-alist
               '("\\*Help\\*" . ((display-buffer-in-side-window)
                                 .
                                 ((side . right)
                                  (window-height . 1)
                                  (window-width . 80)
                                  (inhibit-switch-frame . t)
                                  (inhibit-same-window . t)))))
  (add-to-list 'display-buffer-alist
               '("\\*Warnings\\*" . ((display-buffer-in-side-window)
                                     .
                                     ((side . bottom)
                                      (slot . 1)
                                      (window-height . 10)
                                      (window-width . 1)
                                      (inhibit-switch-frame . t)
                                      (inhibit-same-window . t)))))
  (add-to-list 'display-buffer-alist
               '("\\*Apropos\\*" . ((display-buffer-in-side-window)
                                    .
                                    ((side . right)
                                     (slot . -1)
                                     (window-height . 1)
                                     (window-width . 80)
                                     (inhibit-switch-frame . t)
                                     (inhibit-same-window . t))))))

(provide 'mugu-window-defaults)
;;; mugu-window-defaults ends here
