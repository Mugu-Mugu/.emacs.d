;;; mugu-hydra --- Summary
;; Provide additional dynamic features to hydra packages
;;; Commentary:
;; It is now possible to add a head to an existing hydra.
;; In addition more data about hydra are recorded which allows to replay those.

;;; Code:
(require 'hydra)
(require 'dash)
(require 'general)

(defun mugu-hydra--record-call (name body &optional docstring &rest heads)
  "Advice function that record additional parameters of an hydra:
NAME/BODY and NAME/DOCSTRING.
HEADS are already available and are not recorded"
   (eval `(defvar ,(intern (format "%S/body" name)) body
                "BODY argument that was used for this hydra generation"))
   (eval `(defvar ,(intern (format "%S/docstring" name)) docstring
                "DOSTRING argument that was used for this hydra generation")))

;; record additional parameters on hydra invocation
(advice-add 'defhydra :after #'mugu-hydra--record-call)
(general-unbind 'hydra-base-map "-")

(defun mugu-hydra--list-head-binding (hydra-heads)
  "Return a list of used binding for each heads in HYDRA-HEADS."
  (--map (-first-item it) hydra-heads))

(defun mugu-hydra--add-a-head (name head)
  "Append to hydra named NAME the HEAD.
If HEAD binding conflict with an existing one, the old is deleted and a
warning is logged.
To avoid column issues, HEAD should define its column."
  (let* ((body (symbol-value (intern (format "%S/body" name))))
         (old-heads (symbol-value (intern (format "%S/heads" name))))
         (old-heads-wo-duplicate (if (not (-contains? (mugu-hydra--list-head-binding old-heads) (-first-item head)))
                                     old-heads
                                   (warn "Add hydra head : binding %s of hydra %s overwritten" (-first-item head) name)
                                   (--filter (eq (-first-item it) (-first-item head)) old-heads)))
         (docstring (symbol-value (intern (format "%S/docstring" name)))))
    (eval `(defhydra ,name ,body ,docstring ,@old-heads-wo-duplicate ,head))))

(defun mugu-hydra-add-head (name &rest heads)
  "Append to hydra named NAME all heads HEADS.
If a HEAD binding conflict with an existing one, the old is deleted and a
warning is logged.
To avoid column issues, a HEAD should define its column."
  (-map (lambda (a-head) (mugu-hydra--add-a-head name a-head))
        heads))


(provide 'mugu-hydra)
;;; mugu-hydra ends here
