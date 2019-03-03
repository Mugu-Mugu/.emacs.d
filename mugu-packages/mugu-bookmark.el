(require 'bookmark)
(require 'mugu-directory-fix)

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
