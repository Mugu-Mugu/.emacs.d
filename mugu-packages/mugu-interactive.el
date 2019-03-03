;;; Package --- Summary
;; Provides tools for interactive interface (ie: ivy/helm/ido)
;;; Commentary:

;;; Code:
(require 'hydra)
(require 'mugu-core)

(use-package ivy
  :commands ivy-mode
  :diminish ivy-mode
  :defer
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
  (setq ivy-wrap t)
  (defun ivy-yank-action (x)
    (kill-new x))
  (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
      (if (file-exists-p x)
          (insert (expand-file-name x))
        (insert x))))
  (ivy-set-actions
   t
   '(("p" ivy-copy-to-buffer-action "insert")
     ("y" ivy-yank-action "yank"))))

(use-package smex
  :defer)

(use-package ivy-hydra
  :after ivy
  :commands soo-ivy/body
  :bind
  (:map ivy-minibuffer-map
        ("C-o" . soo-ivy/body)
        ("M-j" . ivy-next-line)
        ("M-k" . ivy-previous-line))
  :config
  (general-def ivy-minibuffer-map
    "<escape>" 'minibuffer-keyboard-quit)
  (require 'ivy)
  (defhydra soo-ivy (:hint nil :color amaranth)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^ ^ _h_ ^+^ _l_ ^ ^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("j" ivy-next-line)
    ("k" ivy-previous-line)
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("C-o" nil)
    ("i" nil)
    ("TAB" ivy-dispatching-call  :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ;; ("d" ivy-done :exit t)
    ("RET" ivy-done :exit t)
    ("C-m" ivy-done :exit t)

    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("o" ivy-occur :exit t))

  (general-def ivy-minibuffer-map
    "<escape>" 'minibuffer-keyboard-quit)

  (after 'evil
    (evil-set-initial-state 'grep-mode 'normal))

  (after 'key-chord
    (key-chord-define ivy-minibuffer-map "jk" 'soo-ivy/body)))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :config
  (counsel-mode +1))

(use-package swiper
  :defer
  :after ivy)

(use-package helm
  :disabled
  :defer
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-split-window-default-side (quote other))
    (setq helm-split-window-in-side-p nil)

    (define-key helm-map (kbd "C-p") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-n") 'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (after 'helm-files
      (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
      (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
      (define-key helm-find-files-map (kbd "C-r") 'helm-find-files-down-last-level))

    (defun helm-jump ()
      "Find files with helm, but be smart about buffers and recent files."
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil))
        (helm-other-buffer '(helm-projectile-sources-list
                             helm-source-buffers-list
                             helm-source-recentf
                             helm-source-bookmarks
                             helm-source-file-cache
                             helm-source-files-in-current-dir
                             helm-source-locate
                             helm-source-buffer-not-found)
                           "*helm jump*")))

    (setq helm-command-prefix-key "C-c h")
    (setq helm-quick-update t)))

(use-package mugu-counsel
  :commands (call-with-fzf-matcher mugu-counsel-find-dir-recursive mugu-counsel-find-file-recursive mugu-counsel-find-anything-recursive mugu-counsel-super-star mugu-counsel-hyper-star)
  :defer
  :straight nil)

(use-package smooth-scrolling
  :defer 4
  :disabled
  :config
  (smooth-scrolling-mode))

(provide 'mugu-interactive)
;;; mugu-interactive ends here
