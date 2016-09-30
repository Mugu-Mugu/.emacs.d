(use-package projectile
  :ensure projectile
  :defer
  :config (progn
            (setq projectile-switch-project-action (lambda ()))
            (setq projectile-enable-caching t)
            (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
            (setq projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld"))
            (add-to-list 'projectile-globally-ignored-directories "elpa")
            (add-to-list 'projectile-globally-ignored-directories ".cache")
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (projectile-global-mode)))

(use-package helm-projectile
  :defer
  :ensure t)

(use-package perspective
  :ensure perspective
  :defer
  :config (progn
            (persp-mode)
            (persp-turn-off-modestring)
            (defvar my-tabs-tab-project-root
              (car projectile-known-projects)
              "A perspective local variable holding the current applicable project directory root"
              )
            (defvar my-tabs-tab-current-directory
              (car projectile-known-projects)
              "A perspective local variable holding the current applicable directory; this is a subdirectory of the project root"
              )
            (my-tabs-init))
  )

(defun my-utils-add-action-to-source (source-symb action-name action-function)
  "add an action whose name is action-name and implementation is action-function to the specified source-symb"
  (let ((modified-source (list)))
    (dolist (source-slot (eval source-symb) modified-source)
      (if (string-equal (car source-slot) "action")
          (progn
            (setq modified-source (add-to-list 'modified-source 
                                               (cons (car source-slot)
                                                     (cons (cons action-name action-function)
                                                           (cdr source-slot))) t))
            )
        (setq modified-source (add-to-list 'modified-source source-slot t))
        )
      )
    modified-source
    )
  )


(defun my-tabs-helm-select-project ()
  (interactive)
  "post treatment procedure called after a new perspective have been created used to init the projectile directory via helm call "
  (helm :sources 
        (helm-build-sync-source "known projects"
          :fuzzy-match t
          :nomark t
          :candidates projectile-known-projects
          :action '(("select" . (lambda (candidate)
                                  (setq my-tabs-tab-current-directory candidate)
                                  (setq my-tabs-tab-project-root candidate)
                                  (setq default-directory candidate)
                                  ))))
        :buffer "select a project"))

(defun my-tabs-helm-action-change-directory (target-directory)
  "A helm action used to change the directory of the current tab"
  (setq my-tabs-tab-current-directory target-directory)
  (setq default-directory target-directory)
  )

(defun my-tabs-helm-action-rename-tab (tab-to-rename)
  "A helm action used to interactivly rename the target perspective"
  (let ((local-tab-name-new (helm-read-string "choose the new perspective name: " tab-to-rename))
        (local-tab-name-current (persp-name persp-curr)))
    (if (equal tab-to-rename (persp-name persp-curr))
        (persp-rename local-tab-name-new)
      (persp-switch tab-to-rename)
      (persp-rename local-tab-name-new)
      (persp-switch local-tab-name-current))))

(defun my-tabs-helm-source-active-tabs ()
  "A helm source gathering active perspectives and relevant action associated"
  (helm-build-sync-source "active perspectives"
    :fuzzy-match t
    :nomark t
    :candidates (persp-all-names)
    :action '(("switch" . (lambda (candidate) (persp-switch candidate)))
              ("rename" . my-tabs-helm-action-rename-tab)
              ("delete" . (lambda (candidate) (persp-delete candidate))))))

(defun my-tabs-helm-source-new-tab ()
  "A dummy helm source to provide action on pattern that didnt match an existing perspective"
  (helm-build-dummy-source "new tab"
    :nomark t
    :action '(("create" . (lambda (candidate)
                            (persp-switch candidate)
                            )))))

(defun my-tabs-helm-main ()
  "Preconfigured `helm' to list perspective or tab action"
  (interactive)
  (let ((local-active-perspectives (my-tabs-helm-source-active-tabs))
        (local-new-perspective     (my-tabs-helm-source-new-tab)))
    (helm :sources '(local-active-perspectives
                     local-new-perspective)
          :buffer "mugu")))

(defun my-tabs-init ()
  "Initialisation function of this custom helm projectile perspective module"
  (persp-make-variable-persp-local 'my-tabs-tab-project-root)
  (persp-make-variable-persp-local 'my-tabs-tab-current-directory)
  (persp-make-variable-persp-local 'default-directory)
  (add-hook 'persp-created-hook 'my-tabs-helm-select-project)
  (setq helm-source-projectile-directories-list
        (my-utils-add-action-to-source 'helm-source-projectile-directories-list "change directory" 'my-tabs-helm-action-change-directory))
  )


(defun my-tabs-helm-projectile-find-file ()
  (interactive)
  (setq default-directory my-tabs-tab-project-root)
  (helm-projectile-find-file)
  )
(defun my-tabs-helm-projectile-find-dir ()
  (interactive)
  (setq default-directory my-tabs-tab-project-root)
  (helm-projectile-find-dir)
  )
(defun my-tabs-helm-projectile ()
  (interactive)
  (setq default-directory my-tabs-tab-project-root)
  (helm-projectile)
  )

(after 'my-hydra
  (defhydra hydra-tabs-menu (:color blue
                                    :hint nil)
    "
^tabs^             
^^^^^^^^---------------------------------------------------
_t_: tabs mgt        
_f_: open file in project
_r_: open recent file in project
_b_: switch to buffer in project
_cd_: browse dir in project
_cr_: change tab root
_e_: look for everythin in project
"
    ("t" my-tabs-helm-main)
    ("r" helm-projectile-recentf)
    ("f" my-tabs-helm-projectile-find-file)
    ("cd" my-tabs-helm-projectile-find-dir)
    ("cr" my-tabs-helm-select-project)
    ("e" my-tabs-helm-projectile)
    ("b" helm-projectile-switch-to-buffer)
    ("q" nil "cancel hydra" :color blue)
    ("SPC" hydra-main-menu/body))
  )

(provide 'mugu-tabs)

;; projectile ne doit pas dependre de perspective
;; projectile feature
;;; recherche fichier sur le repertoire actuel
;;; recherche fichier recursive a partir du repertoire actuel
;;; recherche fichier recursive a partir de la racine
;;; changement recursif repertoire a partir de la racine

