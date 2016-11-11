
(use-package projectile
  :ensure projectile
  :defer
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (after 'ivy (setq projectile-completion-system 'ivy))
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
  (setq projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  (setq projectile-switch-project-action (lambda () nil))
  ;; On project switch, the current project root is recorded
  (advice-add #'projectile-switch-project-by-name :before (lambda (project-file-path &rest args)
                                                            (setq mugu-project-dir-root (file-name-directory project-file-path))))
  ;; All projectile action use this recorded root no matter the current directory
  (advice-add #'projectile-project-root :override (lambda () mugu-project-dir-root))

  ;; this prevent these annoying behaviour ("not in a project" or others automatic project change)
  ;; changing a project should not be performed behind the scene (ie: indirectly through a directory change)

  (projectile-global-mode 1))

(use-package persp-projectile
  :ensure
  :after projectile
  :commands projectile-persp-switch-project
  :config
  (require 'projectile)
  (require 'perspective)
  ;; the root of a project is tied to its workspace
  (persp-make-variable-persp-local 'mugu-project-dir-root))

(use-package mugu-project-ext
  :after projectile
  :commands mugu-project-find-dir
  :config
  (require 'projectile)
  (require 'mugu-directory-fix))

(use-package counsel-projectile
  :disabled ;;; useless??? no added value upon base ivy implementation it lacks history, and different action 
  :after projectile
  :ensure
  :config
  (require 'counsel)
  (counsel-projectile-on))

(defvar mugu-project-dir-root
  nil
  "directory root of the current project")

(after 'hydra
  (defhydra mugu-project-hydra-menu
    (:color blue :hint nil :body-pre (unless mugu-project-dir-root (call-interactively 'projectile-persp-switch-project)))
    "
                              -- PROJECT MENU --

  -> Project Root : %s`mugu-project-dir-root
  -> Current Dir  : %s(mugu-directory-pwd)
"
    ("s" projectile-persp-switch-project "switch project" :color red :column "1-Management")
    ("a" projectile-add-known-project "register new project" :color red)
    ("u" projectile-remove-known-project "unregister project" :color red)
    ("tr" projectile-regenerate-tags "regenerate tags" :column "3-Actions")
    ("i" projectile-invalidate-cache "invalidate cache" :color red)
    ("c" projectile-compile-project "compile project")
    ("v" projectile-vc "version control")
    ("d" mugu-project-find-dir "cd" :color red :column "2-Find")
    ("r" projectile-find-file "open file from all project")
    ("f" (with-mugu-dir 'find-file) "open file current dir")
    ("tf" projectile-find-tag "lookup tag")
    ("g" projectile-grep "grep")
    ("q" nil "quit menu" :color blue :column nil)
    ("SPC" mugu-menu-main-menu "return to main menu" :color blue)))

(provide 'mugu-project)
