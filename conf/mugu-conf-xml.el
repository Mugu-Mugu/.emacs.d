;;; mugu-conf-xml --- Configuration for xml -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package nxml-mode
  :disabled
  :straight nil
  :hook (nxml-mode . lsp)
  :custom
  (lsp-xml-jar-file "/home/david/bin/org.eclipse.lsp4xml-0.3.0-uber.jar")
  (lsp-xml-server-command `("java" "-jar" "/home/david/bin/org.eclipse.lsp4xml-0.3.0-uber.jar")))

(use-package nxml-mode
  :straight nil
  :defer)

(use-package mugu-xml
  :straight nil
  :hook
  (nxml-mode . mugu-xml-activate))

(provide 'mugu-conf-xml)
;;; mugu-conf-xml ends here
