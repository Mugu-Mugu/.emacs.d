;;; mugu-telephone-line --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'telephone-line)
(require 's)

(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)

(defun mugu-modeline-get-flycheck-results (flycheck-report)
  "Return a propertized status of FLYCHECK-REPORT.
It should have form (status . number) where status can be error/warning/info."
  (let ((state (car flycheck-report))
        (count (cdr flycheck-report)))
    (propertize (format "•%s" count)
                'face (intern (format "mugu-modeline-flycheck-%s" state)))))

(telephone-line-defsegment mugu-modeline-flycheck-segment ()
  "Displays current checker state."
  (when (bound-and-true-p flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   ('finished (if flycheck-current-errors
                                  (s-trim (mapconcat 'mugu-modeline-get-flycheck-results
                                                     (flycheck-count-errors flycheck-current-errors) " "))
                                (propertize "✔" 'face '(:foreground "green") 'weight 'extra-bold)))
                   ('running "*")
                   (_ (propertize "x" 'face '(:foreground "red")
                                  'weight 'extra-bold
                                  'mouse-face '(:box 1)
                                  'help-echo (pcase flycheck-last-status-change
                                               ('no-checker "No Checker")
                                               ('not-checked "Not Checked")
                                               ('errored "Error!")
                                               ('interrupted "Interrupted")
                                               ('suspicious "Suspicious?")))))))
      text)))

(telephone-line-defsegment mugu-modeline-buffer-segment ()
  (s-concat (if (buffer-modified-p) "*" "-")
            " %I "
            (propertize (buffer-name) 'face 'bold)))

(telephone-line-defsegment mugu-modeline-vc-segment ()
  (telephone-line-raw
   (if vc-mode
       (s-truncate 20 (s-chop-prefix " Git:" (substring-no-properties vc-mode)) "")
     "")))

  (telephone-line-defsegment mugu-stub-segment ()
    "")

(telephone-line-defsegment mugu-modeline-project-segment ()
  "Displays the current project name, according to projectile."
  (when (fboundp #'projectile-project-name)
    (propertize (projectile-project-name)
                'display '(raise 0.0)
                'help-echo "Switch project"
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda ()
                                       (interactive)
                                       (projectile-switch-project))))))

(defun mugu-telephone-line-set-nocti-theme ()
  "Reconfigure theme for noctilux theme."
  (set-face-attribute 'telephone-line-accent-inactive nil :foreground nil)
  (set-face-attribute 'telephone-line-evil nil :foreground "#3E3D31")
  (set-face-attribute 'telephone-line-evil-insert nil :background "chartreuse3")
  (set-face-attribute 'telephone-line-evil-normal nil :background "darkorange")
  (set-face-attribute 'telephone-line-evil-visual nil :background "gray")
  (set-face-attribute 'telephone-line-evil-replace nil :background "chocolate")
  (set-face-attribute 'telephone-line-evil-motion nil :background "plum3")
  (set-face-attribute 'telephone-line-evil-operator nil :background "Darkgoldenrod2")
  (set-face-attribute 'telephone-line-evil-emacs nil :background "Skyblue2")

  ;; flycheck
  (defface mugu-modeline-flycheck-error
    '((t (:foreground "#FC5C94" :distant-foreground "#A20C41")))
    "Face for flycheck error feedback in the modeline."
    :group 'spaceline)
  (defface mugu-modeline-flycheck-warning
    '((t (:foreground "#F3EA98" :distant-foreground "#968B26")))
    "Face for flycheck warning feedback in the modeline."
    :group 'spaceline)
  (defface mugu-modeline-flycheck-info
    '((t (:foreground "#8DE6F7" :distant-foreground "#21889B")))
    "Face for flycheck info feedback in the modeline."
    :group 'spaceline)

  ;; add lispy
  (defface telephone-line-evil-lispy
    '((t (:background "Skyblue2" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Operator state."
    :group 'telephone-line-evil))


(defun mugu-telephone-line-configure ()
  "Configure telephone line."
  ;; global segment conf
  (setq telephone-line-evil-use-short-tag t)
  (setq telephone-line-primary-left-separator 'telephone-line-identity-right
        telephone-line-secondary-left-separator 'telephone-line-identity-hollow-right
        telephone-line-primary-right-separator 'telephone-line-identity-left
        telephone-line-secondary-right-separator 'telephone-line-identity-hollow-left)
  (setq telephone-line-height 17)

  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (mugu-modeline-buffer-segment))
          (evil . (telephone-line-major-mode-segment))
          (accent . (telephone-line-minor-mode-segment))
          (nil . (mugu-modeline-flycheck-segment))))
  (setq telephone-line-rhs
        '((nil . (mugu-stub-segment))
          (accent . (telephone-line-atom-encoding-segment))
          (evil . (mugu-modeline-vc-segment))
          (accent . (telephone-line-position-segment))
          (evil . (mugu-modeline-project-segment)))))

(provide 'mugu-telephone-line)
;;; mugu-telephone-line ends here
