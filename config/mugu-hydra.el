(require 'hydra)
(require 'dash)

;; various hydra extensions and hacks to implement automatic docstring generation with named columns
(defun hydra--head-set-property (head prop value)
  "set a property in an hydra"
  (cons (car head) (plist-put (cdr head) prop value)))

(defun hydra--head-has-property (head prop)
  "return non nil if head has the property"
  (plist-member (cdr head) prop))

(defun hydra--normalize-heads (heads)
  "ensure each head from HEADS have a property column or set it to the same value as preceding head"
  (let ((current-col nil))
    (-map (lambda (head)
            (if (hydra--head-has-property head :column)
                (setq current-col (hydra--head-property head :column)))
            (hydra--head-set-property head :column current-col))
          heads)))

(defun hydra--sort-heads (normalized-heads)
  "Return a list of non-nil doc heads grouped by identical property column
each heads of NORMALIZED-HEADS must have a column property"
  (--partition-by (hydra--head-property it :column) ;; split the heads into a list of heads grouped by same column
                  (--sort (string< (hydra--head-property it :column) (hydra--head-property other :column)) ;; sort heads by their column property
                          (--filter (nth 2 it) normalized-heads)))) ;; filter out heads with no docsting

(defun hydra--hint-from-matrix (body heads-matrix)
  "generate a formated doc string according to HEADS-MATRIX data and structure"
  (when heads-matrix
    (mapconcat (lambda (heads)
                 (mapconcat (lambda (it)
                              (funcall hydra-key-doc-function
                                       (hydra-fontify-head it body) ;; key
                                       (hydra--head-property it :max-key-len)
                                       (nth 2 it) ;; doc
                                       (hydra--head-property it :max-doc-len)))
                            heads
                            "| "))
               (-partition-all (length heads-matrix) (apply '-interleave heads-matrix)) "\n")))

(defun hydra--generate-matrix (heads-grouped-by-col)
  "Return a square matrix from HEADS-GROUPED-BY-COL with padding heads where applicable
a property max-key-len and max-doc-len is also applied to each head and represents the maximum dimension of the column"
  (when heads-grouped-by-col
    (-map (lambda (heads) "compute the max key/doc size for this heads list and apply it to each head of this group"
            (let* ((column-name (hydra--head-property (-first-item heads) :column))
                   (heads-with-header (-clone (-insert-at 0 `(" " nil ,column-name :column ,column-name :exit t) heads)))
                   (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) heads-with-header)))
                   (max-doc-len (apply #'max (mapcar (lambda (x) (length (hydra--to-string (nth 2 x)))) heads-with-header)))
                   (heads-full-header (-clone (-insert-at 1 `(" " nil ,(make-string (+ 2 max-key-len max-doc-len) ?-) :exit t) heads-with-header))))
              (--map (progn (hydra--head-set-property it :max-key-len max-key-len)
                            (hydra--head-set-property it :max-doc-len max-doc-len))
                     heads-full-header)))
          (-clone (apply '-pad '(" " nil " " :exit t) heads-grouped-by-col)))))

(defun mugu/hydra--hint (original-hydra-hint body heads)
  (let* ((sorted-heads (hydra--sort-heads (hydra--normalize-heads heads)))
         (heads-w-col (--filter (hydra--head-property (-first-item it) :column) sorted-heads))
         (heads-wo-col  (-flatten-n 1 (--remove (hydra--head-property (-first-item it) :column) sorted-heads))))
    (concat "\n" (hydra--hint-from-matrix body (hydra--generate-matrix heads-w-col))
            "\n"
            (funcall original-hydra-hint body heads-wo-col))))

(defun mugu/hydra-fontify (head body)
  "Produce a pretty string from HEAD and BODY.
HEAD's binding is returned as a string with a colored face."
  (cond ((eq " " (nth 0 head)) " ")
        (t (hydra-fontify-head-default head body))))

(defun mugu/hydra-doc-format (key key-width doc doc-width)
  "Doc"
  (cond
   ((equal key " ") (format (format "%%-%ds" (+ 2 key-width doc-width)) doc))
   (t (format (format "%%%ds %%%ds" key-width (- -1 doc-width)) key doc))))

(setq hydra-fontify-head-function 'mugu/hydra-fontify)
(setq hydra-key-doc-function 'mugu/hydra-doc-format)
(advice-add #'hydra--hint :around #'mugu/hydra--hint)

(provide 'mugu-hydra)
