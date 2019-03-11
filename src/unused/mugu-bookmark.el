;;; mugu-bookmark --- Summary
;; tbc
;;; Commentary:

;;; Code:

(require 'bookmark)
(require 'mugu-directory)
(require 'use-package)

(use-package bookmark
  :defer t)

(use-package mugu-bookmark-utils
  :defer t
  :straight nil
  :commands
  mugu-bookmark-register-dir
  mugu-bookmark-load-file
  mugu-bookmark-load-dir)

(provide 'mugu-bookmark)
;;; mugu-bookmark ends here
