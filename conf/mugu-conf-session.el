;;; mugu-conf-session --- Summary
;; tbc
;;; Commentary:

;;; Code:

;; * begin:
(require 'use-package)

(use-package saveplace
  :config
  (save-place-mode))

(use-package savehist
  :custom
  (savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-autosave-interval 60)
  :config
  (savehist-mode t))

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 500)
  :config
  (recentf-mode 1))

(provide 'mugu-conf-session)
;;; mugu-conf-session ends here
