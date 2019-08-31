;;; mugu-yafold --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-menu)
(require 'yafolding)

(defmenu mugu-yafold-main (:color red :hint nil :body-pre (require 'yafolding))
  "Bindings for general folding (code and outline)."
  ("<tab>" yafolding-toggle-element "toggle-element" :column "Local")
  ("o" yafolding-show-element "show-element")
  ("c" yafolding-hide-element "hide-element")
  ("C" yafolding-hide-parent-element "hide-parent-element")
  ("ro" yafolding-show-region "show-region" :column "On region")
  ("rc" yafolding-hide-region "hide-region")
  ("<S-tab>" yafolding-toggle-all "toggle-all" :column "Global")
  ("zo" yafolding-show-all "show-all")
  ("zc" yafolding-hide-all "hide-all"))

(defun mugu-yafold-activate ()
  "Activate the yafold feature."
  (add-hook 'prog-mode-hook 'yafolding-mode))

(provide 'mugu-yafold)
;;; mugu-yafold ends here
