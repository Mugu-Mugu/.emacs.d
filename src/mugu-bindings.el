;;; mugu-bindings --- Utility forbindings -*- lexical-binding: t -*-
;;; Commentary:

(require 'dash)
(require 's)
(require 'which-key)

;;; Code:
(defun mugu-bindings-generate-hydra-summonning (hydra-name bindings)
  "Return the blueprint to invoke an hydra corresponding to the provided args.
Hydra will be named with HYDRA-NAME and heads will be generated from BINDINGS,
an alist of (key command)."
  (let* ((hydra-name-sym (intern hydra-name))
         (column-count (min 5 (+ 1 (/ (length bindings) 5))))
         (column-size (/ (length bindings)
                         column-count))
         (make-heads (-lambda (index (keys . command))
                       (list (format "%s" keys)
                             (intern (format "%s" command))
                             (format "%s" command)
                             :column (format "Normal %s" (/ index column-size)))))
         (heads (->> bindings
                  (-sort (-on #'s-less? 'car))
                  (-map-indexed make-heads)))
         (hydra-incantation `(defhydra ,hydra-name-sym
                               (:color red :hint nil)
                               ,@heads
                               ("q" nil "quit" :color blue :column nil))))
    hydra-incantation))


(defun mugu-bindings-hydrate (keymap-symbol)
  "Hydrate the keymap corresponding to KEYMAP-SYMBOL."
  (-let* ((keymap (symbol-value keymap-symbol))
          (keymap-name (symbol-name keymap-symbol))
          (bindings (mugu-bindings-compute-virtual-map keymap))
          (hydra-name (format "%s-hydrated" keymap-name)))
    (eval (mugu-bindings-generate-hydra-summonning hydra-name bindings))))

(defun mugu-bindings-compute-virtual-map (keymap)
  "Return a list of applicable bindings for KEYMAP while considering evil state."
  (let* ((prefix-binding? (-lambda ((_ . command)) (equal command "Prefix Command")))
         (prefix-keys (->> (which-key--get-keymap-bindings keymap nil)
                        (-select prefix-binding?)
                        (-map 'car)))
         (prefix-key? (-lambda ((keys . _)) (-contains? prefix-keys keys)))
         (normalize-binding (-lambda ((keys . command))
                              (if (not (or (s-contains? "C-" keys)
                                           (s-contains? "M-" keys)))
                                  (cons (s-replace " " "" keys) command)
                                (cons keys command)))))
    (->> (which-key--get-keymap-bindings keymap 'all)
      (-map normalize-binding)
      (-reject prefix-key?))))

(defun mugu-bindings-default-major-mode-menu ()
  "Return a default menu hydra for the current major mode."
  (let* ((major-mode-keymap-symbol (intern (format "%s-map" major-mode)))
         (major-mode-default-hydra-symbol (intern (format "%s/body" major-mode-keymap-symbol)))
         (major-mode-default-hydra (or (and (functionp major-mode-default-hydra-symbol)
                                            major-mode-default-hydra-symbol)
                                       (mugu-bindings-hydrate major-mode-keymap-symbol))))
    (funcall major-mode-default-hydra)))

(provide 'mugu-bindings)
;;; mugu-bindings ends here
