(use-package projectile
  :ensure projectile
  :defer
  :diminish
  :config 
  (setq projectile-enable-caching t)
  (after 'ivy (setq projectile-completion-system 'ivy))
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
  (setq projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld"))
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  ;; On project switch, the current project root is recorded
  (advice-add #'projectile-switch-project-by-name :before (lambda (project-file-path &rest args)
                                                            (setq mugu-project-dir-root (file-name-directory project-file-path))))
  ;; All projectile action use this recorded root no matter the current directory
  (advice-add #'projectile-project-root :override (lambda () mugu-project-dir-root))
  ;; this prevent these annoying behaviour ("not in a project" or others automatic project change)
  ;; changing a project is no trivial matter and should not be performed behind the scene (ie: indirectly through a directory change)

  (projectile-global-mode +1)
  )

(use-package persp-projectile
  :ensure
  :after projectile
  :commands projectile-persp-switch-project
  :config
  (require 'projectile)
  (require 'perspective)
  ;; the root of a project is tied to its workspace
  (persp-make-variable-persp-local 'mugu-project-dir-root)
  )

(use-package mugu-project-ext
  :after projectile
  :commands mugu-project-find-dir
  :config
  (require 'projectile)
  (require 'mugu-directory-fix)
  )

(defvar mugu-project-dir-root
  nil
  "directory root of the current project")

(after 'hydra
  (defhydra mugu-project-hydra-menu (:color blue :hint nil)
    "
^project^              ^find^             
_s_: switch project    _d_: find directory
_c_: compile project   _f_: find file
_r_: regenerate tag    _t_: find tag
_v_: version control   _g_: find pattern
"
    ("s" projectile-persp-switch-project)
    ("d" mugu-project-find-dir)
    ("f" projectile-find-file)
    ("t" projectile-find-tag)
    ("c" projectile-compile-project)
    ("r" projectile-regenerate-tags)
    ("g" projectile-grep)
    ("v" projectile-vc)
    ("q" nil "quit menu" :color blue)
    ("SPC" hydra-main-menu/body "return to main menu"))
  )

(provide 'mugu-project)
