;;; mugu-search --- Binding for various search engine-*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'ivy)
(require 'ol)
(require 's)
(require 'dash)

(defun mugu-org-link--decompose-candidate (candidate)
  "Extract link and description from CANDIDATE.
Return (link description)"
  (reverse (s-split "\n" (substring-no-properties candidate))))

(defun mugu-org-link--action-store (candidate)
  "Store the CANDIDATE links into org."
  (push (mugu-org-link--decompose-candidate candidate)
        org-stored-links))

(defun mugu-org-link--action-insert (candidate)
  "Insert the CANDIDATE link as a org format link."
  (message "%s" (mugu-org-link--decompose-candidate candidate))
  (apply #'org-insert-link nil (mugu-org-link--decompose-candidate candidate)))

(defun mugu-org-link-http-complete ()
  "Interactively insert a link with completion.
Hacky but org doesn't provide a clean interface :/."
  (require 'counsel-web)
  (ignore-errors
    (let ((counsel-web-search-action #'mugu-org-link--action-insert))
      (counsel-web-suggest)
      (exit-minibuffer)
      (normal-mode))))

(defun mugu-org-link-activate-ivy-actions ()
  "."
  (ivy-add-actions 'counsel-web-search
                   '(("s" mugu-org-link--action-store "store org link")
                     ("i" mugu-org-link--action-insert "insert org link"))))


(provide 'mugu-org-link)
;;; mugu-org-link ends here
