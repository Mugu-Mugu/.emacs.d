;;; mugu-conf-writing --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  ;; (prog-mode . flyspell-prog-mode)
  :custom
  (ispell-quietly t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (ispell-extra-args ("--sug-mode=ultra") )
  :config
  (mugu-silence-function #'ispell-lookup-words)
  ;; fuck this horrible flyspell hack
  (advice-add 'flyspell-after-change-function :around
              (lambda (&rest args)
                (remove-hook 'after-change-functions #'flyspell-after-change-function t)))

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

(use-package flyspell-lazy
  :disabled "triggers some error and also kill buffers randomly."
  :after flyspell
  :config
  (flyspell-lazy-mode))

(use-package guess-language
  :after flyspell
  :hook
  (text-mode . guess-language-mode)
  :custom
  (guess-language-min-paragraph-length 45)
  :config
  (mugu-silence-function #'guess-language)
  (setq guess-language-languages '(en fr))
  :diminish guess-language-mode)

(use-package company
  :disabled "In practice, orthographic completion checking is not effective. Stopping to read a word and the possible candidates is still slower than typing fast and wrong and correcting later."
  :after guess-language
  :config
  (add-to-list 'company-backends '(company-dabbrev company-ispell company-yasnippet) 'append)
  (add-to-list #'guess-language-after-detection-functions
               (lambda (&rest _args) (setq-local company-ispell-dictionary (f-join (expand-file-name user-emacs-directory) "etc/spelling" (concat (or ispell-current-dictionary "default") ".txt"))))
               'append))


(provide 'mugu-conf-writing)
;;; mugu-conf-writing ends here
