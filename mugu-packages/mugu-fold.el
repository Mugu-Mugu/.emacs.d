;;; mugu-fold --- Summary
;; Handle fold and general outline management
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'general)
(require 'mugu-menu)

(use-package outline
  :defer t
  :delight outline-minor-mode)

(use-package outshine
  :defer t
  :delight outshine-mode
  :hook  ((emacs-lisp-mode) . outshine-mode))

(use-package origami
  :defer t
  :config
  (global-origami-mode)
  (defun mugu-origami-elisp-parser (create)
    (origami-lisp-parser create "(\\(def\\|use-package\\)\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))
  (add-to-list 'origami-parser-alist '(emacs-lisp-mode . mugu-origami-elisp-parser)))

(defmacro mugu-fold-make-command (cmd-name outline-cmd origami-cmd)
  "Make a command named mugu-fold-[CMD-NAME] that dispatch to the correct fold.
OUTLINE-CMD is called when on a header outline, otherwse ORIGAMI-CMD is called."
  `(defun ,(intern (format "mugu-fold-%s" cmd-name)) ()
     ,(format "Do %s if on a outline headline, else do %s" outline-cmd origami-cmd)
     (interactive)
     (require 'outshine)
     (require 'origami)
     (cond ((outline-on-heading-p) (call-interactively ,outline-cmd))
           ((mugu-fold-line-with-fold?) (call-interactively ,origami-cmd))
           ((or (bound-and-true-p origami-mode)
                (bound-and-true-p outshine-mode))
            (mugu-fold-menu))
           (t (message "No fold or outline in this file")))))

(mugu-fold-make-command open-recursive #'outline-show-subtree #'origami-open-node-recursively)
(mugu-fold-make-command close-recursive #'outline-hide-subtree #'origami-close-node-recursively)
(mugu-fold-make-command focus #'outline-hide-other #'origami-show-only-node)
(mugu-fold-make-command toggle #'outshine-cycle #'origami-recursively-toggle-node)

;;*
(defmenu mugu-fold-menu
  (:color red :hint nil :body-pre (and (require 'outshine) (require 'origami)))
  "Bindings for general folding (code and outline)."
  ("o" (mugu-fold-open-recursive) "open recursively" :column "Folding at point")
  ("c" (mugu-fold-close-recursive) "close recursively")
  ("a" (mugu-fold-toggle) "toogle visibiliy")
  ("u" origami-undo "undo last fold")
  ("r" origami-redo "redo last fold")
  ("<tab>" (mugu-fold-toggle) "toogle visibiliy")
  ("f" (mugu-fold-focus) "focus" :color blue)
  ("zo" origami-reset "open all recursively" :column "Folding Global")
  ("zc" origami-close-all-nodes "close all recursively")
  ("za" origami-toggle-all-nodes "toogle all")
  ("io" (outline-show-all) "open all recursively" :column "Outline")
  ("ic" (outline-hide-body) "close all recursively")
  ("ia" (outshine-cycle-buffer) "toogle all")
  ("ii" (counsel-outline) "jump to")
  ("h" (outline-previous-heading) "↑ outline" :column "Navigation")
  ("j" (mugu-fold-next-fold) "↓ fold")
  ("k" (mugu-fold-prev-fold) "↑ fold")
  ("l" (outline-next-heading) "↓ outline")
  ("zz" (recenter) "recenter view")
  ("q" nil "exit" :color blue :column nil))

(defun mugu-fold-line-with-fold? ()
  "Return t if point is on a origami fold."
  (-when-let (tree (origami-get-fold-tree (current-buffer)))
    (--first (and (>= (line-number-at-pos (point))
                      (line-number-at-pos (+ (origami-fold-beg it) (origami-fold-offset it))))
                  (<= (line-number-at-pos (point))
                      (line-number-at-pos (origami-fold-end it))))
             (origami-fold-children tree))))

(defun mugu-fold-next-fold ()
  "Go to begining of next fold."
  (interactive)
  (-when-let (tree (origami-get-fold-tree (current-buffer)))
    (-when-let (next-fold (--first (and (< (line-number-at-pos (point))
                                           (line-number-at-pos (+ (origami-fold-beg it) (origami-fold-offset it)))))
                                   (origami-fold-children tree)))
      (goto-char (+ (origami-fold-beg next-fold) (origami-fold-offset next-fold))))))

(defun mugu-fold-prev-fold ()
  "Go to begining of previous fold."
  (interactive)
  (-when-let (tree (origami-get-fold-tree (current-buffer)))
    (-when-let (prev-fold (--first (and (> (line-number-at-pos (point))
                                           (line-number-at-pos (+ (origami-fold-beg it) (origami-fold-offset it)))))
                                   (reverse (origami-fold-children tree))))
      (goto-char (+ (origami-fold-beg prev-fold) (origami-fold-offset prev-fold))))))

(general-define-key
 :keymaps 'global
 :states 'motion
 :predicate '(not (eq major-mode 'org-mode))
 "<tab>" (general-key-dispatch #'mugu-fold-toggle
           :timeout 0.2
           "<tab>" 'mugu-fold-menu))

(provide 'mugu-fold)
;;; mugu-fold ends here
