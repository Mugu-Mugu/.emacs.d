;;; mugu-modeline --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)
(require 'mugu-cosmetics)

(use-package telephone-line
  :after evil
  :config
  (require 'all-the-icons)

  ;; reconfigure theme
  (set-face-attribute 'telephone-line-accent-inactive nil :foreground nil)
  (set-face-attribute 'telephone-line-evil nil :foreground "#3E3D31")
  (set-face-attribute 'telephone-line-evil-insert nil :background "chartreuse3")
  (set-face-attribute 'telephone-line-evil-normal nil :background "darkorange")
  (set-face-attribute 'telephone-line-evil-visual nil :background "gray")
  (set-face-attribute 'telephone-line-evil-replace nil :background "chocolate")
  (set-face-attribute 'telephone-line-evil-motion nil :background "plum3")
  (set-face-attribute 'telephone-line-evil-operator nil :background "Darkgoldenrod2")
  (set-face-attribute 'telephone-line-evil-emacs nil :background "Skyblue2")
  (set-face-attribute 'telephone-line-projectile nil :foreground nil :background nil)

  ;; add lispy
  (defface telephone-line-evil-lispy
    '((t (:background "Skyblue2" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Operator state."
    :group 'telephone-line-evil)

  ;; global segment conf
  (setq telephone-line-evil-use-short-tag t)
  (setq telephone-line-primary-left-separator 'telephone-line-identity-right
        telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
        telephone-line-primary-right-separator 'telephone-line-identity-left
        telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)
  (setq telephone-line-height 17)

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
                                  (propertize "✔" 'face '(:foreground "green" :weight "bold"))))
                     ('running "*")
                     (_ (propertize "x" 'face '(:foreground "red"
                                                            :weight "bold")

                                    'mouse-face '(:box 1)
                                    'help-echo (pcase flycheck-last-status-change
                                                 ('no-checker "No Checker")
                                                 ('not-checked "Not Checked")
                                                 ('errored "Error!")
                                                 ('interrupted "Interrupted")
                                                 ('suspicious "Suspicious?")))))))
        text)))


  (telephone-line-defsegment mugu-modeline-buffer-segment ()
    (telephone-line-raw (s-concat (if (buffer-modified-p) "*" "-")
                                  " %I "
                                  (propertize (buffer-name) 'face 'bold))
                        t))

  (defun -custom-modeline-github-vc ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       ;; (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
       ;; " · "
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9)))))
  (telephone-line-defsegment mugu-modeline-vc-segment ()
    (if vc-mode
        (cond
         ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
         (t (format "%s" vc-mode)))
      ""))

  ;; segments conf
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (mugu-modeline-buffer-segment))
          (evil . (telephone-line-major-mode-segment
                     telephone-line-minor-mode-segment))
          (nil . (mugu-modeline-flycheck-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-vc-segment
                  telephone-line-projectile-segment))
          (accent . (telephone-line-atom-encoding-segment))
          (evil . (telephone-line-position-segment))))
  (telephone-line-mode 1))

(use-package spaceline
  :demand
  :disabled
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; (spaceline-spacemacs-theme)
  (setq spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t))


(provide 'mugu-modeline)
;;; mugu-modeline ends here
