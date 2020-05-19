;;; mugu-org-protocol --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

(require 'org-protocol)

(defun mugu-org-protocole-capture (info)
  "Process an org-protocol://mugu-capture?ref= style url with INFO.

It opens or creates a note with the given ref.

  javascript:location.href = \\='org-protocol://roam-ref?template=r&ref=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())"
  (when-let* ((alist (org-roam--plist-to-alist info))
              (decoded-alist (mapcar (lambda (k.v)
                                       (let ((key (car k.v))
                                             (val (cdr k.v)))
                                         (cons key (org-link-decode val)))) alist)))
    (unless (assoc 'ref decoded-alist)
      (error "No ref key provided"))
    (when-let ((title (cdr (assoc 'title decoded-alist))))
      (push (cons 'slug (org-roam--title-to-slug title)) decoded-alist))
    (let* ((org-roam-capture-templates org-roam-capture-ref-templates)
           (org-roam-capture--context 'ref)
           (org-roam-capture--info decoded-alist)
           (template (cdr (assoc 'template decoded-alist))))
      (raise-frame)
      (org-roam--capture nil template)
      (message "Item captured.")))
  nil)

;;; Code:

(provide 'mugu-org-protocol)
;;; mugu-org-protocol ends here
