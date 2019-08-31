;;; mugu-origami --- Origami/Outshine wrapper and enhancer -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'origami)
(require 'mugu-menu)
(require 'general)

(defun mugu-origami-toggle-node-and-activate ()
  "Activate origami mode if needed and toggle node at point."
  (interactive)
  (unless origami-mode
    (origami-mode 1))
  (call-interactively #'origami-recursively-toggle-node))

;;*
(defmenu mugu-origami-menu
  (:color red :hint nil
          :body-pre
          (and (require 'origami)
               (unless origami-mode (origami-mode 1))))
  "Bindings for general folding (code and outline)."
  ("o" origami-open-node-recursively "open recursively" :column "Folding at point")
  ("c" origami-close-node-recursively "close recursively")
  ("a" origami-recursively-toggle-node "toogle visibiliy")
  ("<tab>" origami-recursively-toggle-node "toogle visibiliy")
  ("f" origami-show-only-node "focus" :color blue)
  ("u" origami-undo "undo last fold" :column "Undo/Redo")
  ("r" origami-redo "redo last fold")
  ("zo" origami-reset "open all recursively" :column "Folding Global")
  ("zc" origami-close-all-nodes "close all recursively")
  ("za" origami-toggle-all-nodes "toogle all")
  ;; ("ii" (counsel-outline) "jump to")
  ("j" (mugu-origami-next-fold) "↓ fold" :column "Navigation")
  ("k" (mugu-origami-prev-fold) "↑ fold")
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
