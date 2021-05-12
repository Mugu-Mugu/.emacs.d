;;; mugu-conf-writing --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (ispell-quietly t)
  (flyspell-issue-message-flag nil)
  :pretty-hydra
  (mugu-flyspell-menu (:color red :hint nil)
                      ("Search error"
                       (("<tab>" flyspell-goto-next-error "next error")
                        ("B" flyspell-buffer "check buffer"))
                       "Fix"
                       ((";" flyspell-correct-wrapper "backward")
                        ("." (progn (setq current-prefix-arg '(4)) (flyspell-correct-wrapper)) "all backward")
                        (":" (progn (setq current-prefix-arg '(8)) (flyspell-correct-wrapper)) "forward")
                        ("/" (progn (setq current-prefix-arg '(16)) (flyspell-correct-wrapper)) "all forward"))
                       "Other"
                       (("D" (flyspell-mode-off) "disable")))))

(use-package flyspell-correct
  :after flyspell
  :general
  (:keymaps 'flyspell-mode-map
            "C-;" #'flyspell-correct-wrapper))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package guess-language
  :hook
  (text-mode . guess-language-mode)
  :custom
  (guess-language-min-paragraph-length 45)
  :config
  (setq guess-language-languages '(en fr))
  :diminish guess-language-mode)

(use-package company
  :after guess-language
  :config
  (add-to-list #'guess-language-after-detection-functions
               (lambda (&rest _args) (setq-local company-ispell-dictionary (f-join (expand-file-name user-emacs-directory) "etc/spelling" (concat (or ispell-current-dictionary "default") ".txt"))))
               'append))


(provide 'mugu-conf-writing)
;;; mugu-conf-writing ends here
