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
  (setq last-command #'origami-recursively-toggle-node)
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
  ("h" origami-backward-fold-same-level "↓ fold same level" :column "Navigation")
  ("j" origami-forward-fold "↓ fold")
  ("k" origami-previous-fold "↑ fold")
  ("l" origami-forward-fold-same-level "↑ fold same level")
  ("zz" (recenter) "recenter view")
  ("q" nil "exit" :color blue :column nil))

(general-define-key :states 'motion
                    "<f5>" #'origami-recursively-toggle-node)

(provide 'mugu-origami)
;;; mugu-origami ends here
