(require 'hydra)

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

;; (defmacro mugu-hydra-add-head (name &rest head)
;;   "Append to hydra named NAME the HEAD."
;;   (let* ((body (symbol-value (intern (format "%S/body" name))))
;;          (body-with-inherit (plist-put body :inherit (list (intern (format "%S/heads" name)))))
;;          (docstring (symbol-value (intern (format "%S/docstring" name)))))
;;     `(defhydra ,name ,body-with-inherit ,docstring ,(car head))))

(defun mugu-hydra-add-head (name &rest head)
  "Append to hydra named NAME the HEAD."
  (let* ((body (symbol-value (intern (format "%S/body" name))))
         (body-with-inherit (plist-put body :inherit (list (intern (format "%S/heads" name)))))
         (docstring (symbol-value (intern (format "%S/docstring" name)))))
    (eval `(defhydra ,name ,body-with-inherit ,docstring ,(car head)))))

(provide 'mugu-hydra)
