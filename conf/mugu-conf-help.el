;;; mugu-conf-help --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package helpful)
(use-package helpful
  :after ivy
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))
(use-package helpful
  :after mugu-window
  :config
  (mugu-window-configure-side-window "\\*helpful.*\\*" 'right 80))

(provide 'mugu-conf-help)
;;; mugu-conf-help ends here
