;;; mugu-origami --- Origami/Outshine wrapper and enhancer -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'origami)
(require 'mugu-menu)
(require 'outline)
(require 'outshine)
(require 'general)

(defun mugu-origami-elisp-parser (create)
  "Slightly improved elisp parser.
I have no idea what CREATE is."
  (origami-lisp-parser create "(\\(def\\|use-package\\)\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

(defun mugu-origami-activate ()
  "Activate origami conf."
  (general-define-key
   :keymaps 'global
   :states 'motion
   :predicate '(not (eq major-mode 'org-mode))
   "<tab>" (general-key-dispatch #'mugu-fold-toggle
             :timeout 0.2
             "<tab>" 'mugu-fold-menu))

  (add-to-list 'origami-parser-alist '(emacs-lisp-mode . mugu-origami-elisp-parser)))


(defmacro mugu-origami-make-command (cmd-name outline-cmd origami-cmd)
  "Make a command mugu-origami-[CMD-NAME] that dispatch to the correct fold.
OUTLINE-CMD is called when on a header outline, otherwse ORIGAMI-CMD is called."
  `(defun ,(intern (format "mugu-origami-%s" cmd-name)) ()
     ,(format "Do %s if on a outline headline, else do %s" outline-cmd origami-cmd)
     (interactive)
     (require 'origami)
     (cond ;; ((outline-on-heading-p) (call-interactively ,outline-cmd))
           ((mugu-origami-line-with-fold?) (call-interactively ,origami-cmd))
           ((or (bound-and-true-p origami-mode)
                (bound-and-true-p outshine-mode))
            (mugu-origami-menu))
           (t (message "No fold or outline in this file")))))

(mugu-origami-make-command open-recursive #'outline-show-subtree #'origami-open-node-recursively)
(mugu-origami-make-command close-recursive #'outline-hide-subtree #'origami-close-node-recursively)
(mugu-origami-make-command focus #'outline-hide-other #'origami-show-only-node)
(mugu-origami-make-command toggle #'outshine-cycle #'origami-recursively-toggle-node)

;;*
(defmenu mugu-origami-menu
  (:color red :hint nil :body-pre (require 'origami))
  "Bindings for general folding (code and outline)."
  ("o" (mugu-origami-open-recursive) "open recursively" :column "Folding at point")
  ("c" (mugu-origami-close-recursive) "close recursively")
  ("a" (mugu-origami-toggle) "toogle visibiliy")
  ("u" origami-undo "undo last fold")
  ("r" origami-redo "redo last fold")
  ("<tab>" (mugu-origami-toggle) "toogle visibiliy")
  ("f" (mugu-origami-focus) "focus" :color blue)
  ("zo" origami-reset "open all recursively" :column "Folding Global")
  ("zc" origami-close-all-nodes "close all recursively")
  ("za" origami-toggle-all-nodes "toogle all")
  ;; ("ii" (counsel-outline) "jump to")
  ;; ("h" (outline-previous-heading) "↑ outline" :column "Navigation")
  ("j" (mugu-origami-next-fold) "↓ fold")
  ("k" (mugu-origami-prev-fold) "↑ fold")
  ;; ("l" (outline-next-heading) "↓ outline")
  ("zz" (recenter) "recenter view")
  ("q" nil "exit" :color blue :column nil))

(defun mugu-origami-line-with-fold? ()
  "Return t if point is on a origami fold."
  (-when-let (tree (origami-get-fold-tree (current-buffer)))
    (--first (and (>= (line-number-at-pos (point))
                      (line-number-at-pos (+ (origami-fold-beg it) (origami-fold-offset it))))
                  (<= (line-number-at-pos (point))
                      (line-number-at-pos (origami-fold-end it))))
             (origami-fold-children tree))))

(defun mugu-origami-next-fold ()
  "Go to begining of next fold."
  (interactive)
  (-when-let (tree (origami-get-fold-tree (current-buffer)))
    (-when-let (next-fold (--first (and (< (line-number-at-pos (point))
                                           (line-number-at-pos (+ (origami-fold-beg it) (origami-fold-offset it)))))
                                   (origami-fold-children tree)))
      (goto-char (+ (origami-fold-beg next-fold) (origami-fold-offset next-fold))))))

(defun mugu-origami-prev-fold ()
  "Go to begining of previous fold."
  (interactive)
  (-when-let (tree (origami-get-fold-tree (current-buffer)))
    (-when-let (prev-fold (--first (and (> (line-number-at-pos (point))
                                           (line-number-at-pos (+ (origami-fold-beg it) (origami-fold-offset it)))))
                                   (reverse (origami-fold-children tree))))
      (goto-char (+ (origami-fold-beg prev-fold) (origami-fold-offset prev-fold))))))



(provide 'mugu-origami)
;;; mugu-origami ends here
