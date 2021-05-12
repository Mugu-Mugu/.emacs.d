;;; mugu-org-protocol --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
(require 'org-protocol)
(require 'org-protocol-capture-html)
(require 'mugu-org-sql)
(require 'mugu-org-utils)

(defcustom mugu-orgp-capture-file-path (file-truename "~/org/capture.org")
  "File where the org capture will happen."
  :type 'string
  :group 'mugu)

(defcustom mugu-orgp-capture-default-inbox "inbox"
  "Title of the headline where the capture will happen."
  :type 'string
  :group 'mugu)

 (defun mugu-orgp-goto-capture-headline ()
  "Goto location where capture should happen."
  (let ((captured-title (plist-get org-store-link-plist :annotation)))
    (mugu-orgu-goto-headline
     (or (mugu-orgu-find-headline-in mugu-orgp-capture-file-path captured-title)
         (mugu-orgu-find-headline-in mugu-orgp-capture-file-path mugu-orgp-capture-default-inbox)))))

;;; Code:
 (provide 'mugu-org-protocol)
;;; mugu-org-protocol ends here
