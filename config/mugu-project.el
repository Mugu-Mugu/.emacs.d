(use-package projectile
  :ensure projectile
  :defer
  :diminish
  :config (progn
            (setq projectile-enable-caching t)
            (after 'ivy (setq projectile-completion-system 'ivy))
            (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
            (setq projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld"))
            (add-to-list 'projectile-globally-ignored-directories "elpa")
            (add-to-list 'projectile-globally-ignored-directories ".cache")
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (projectile-global-mode)))

(use-package persp-projectile
  :ensure
  :defer
  :after perspective
  :commands projectile-persp-switch-project
  :config
  (ivy-mode +1)
  )

(after 'hydra
  (defhydra mugu-project-hydra-menu (:color blue
                                              :hint nil)
    "
^project^              ^find^             
_s_: switch project    _d_: find directory
_c_: compile project   _f_: find file
_r_: regenerate tag    _t_: find tag
_v_: version control   _g_: find pattern
"
    ("s" projectile-persp-switch-project)
    ("d" projectile-find-dir)
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
