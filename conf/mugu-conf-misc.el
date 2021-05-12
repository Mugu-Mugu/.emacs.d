;;; mugu-conf-misc --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:
(require 'use-package)

;;; Code:
(use-package mugu-ea
  :demand
  :straight nil
  :hook
  (ea-popup . mugu-popup-handler))

(provide 'mugu-conf-misc)
;;; mugu-conf-misc ends here
