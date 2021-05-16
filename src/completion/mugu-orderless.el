;;; mugu-orderless --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'orderless)
(require 'company)

(defun mugu-orderless-flex-if-twiddle (pattern _index _total)
  "After a completion, allow to use flex PATTERN matching if twiddle."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun mugu-orderless-first-with-initialism (_pattern index _total)
  "On first completion, also consider initialism as possible matching style.
Only work on first INDEX as initial match is unique by definition."
  (when (= index 0) `(orderless-initialism ,@orderless-matching-styles)))

(defun mugu-orderless-without-if-bang (pattern _index _total)
  "Allow to negate a PATTERN if requested.
It makes sense to use it on front:
- it's typically use would be to exclude some pattern from the match list
  and this can be done only after the fact.
- usually bang never appears in front of something we want to match.
But does not for the back:
- some language like ruby has symbols that ends with a bang
- completion is about what I want to look for.  It makes no sense to ask for
something only to negate it at the end."
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

(defun mugu-orderless-company-face-advice (fn &rest args)
  "Advice FN to display matching face for company.
ARGS."
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(provide 'mugu-orderless)
;;; mugu-orderless ends here
