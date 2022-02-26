;;; mugu-org-features --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'mugu-feature)

(define-mugu-feature org-note)
(define-mugu-feature org-insert-link-note)

(define-mugu-feature org-view-active-tasks "tasks currently active")
(define-mugu-feature org-goto-planification-note "goto the note where the workflow is located")
(define-mugu-feature org-goto-setupfile "goto the file where the common org settings is located")

(provide 'mugu-org-feature)
;;; mugu-org-features ends here
