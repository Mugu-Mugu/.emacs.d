;;; mugu-search --- Binding for various search engine-*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'engine-mode)
(require 'format-spec)

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(provide 'mugu-search)
;;; mugu-search ends here
