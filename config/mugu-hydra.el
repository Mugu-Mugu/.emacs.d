(require 'hydra)
(require 'cl-lib)

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
    (mapcar (lambda (head)
            (if (hydra--head-has-property head :column)
                (setq current-col (hydra--head-property head :column)))
            (hydra--head-set-property head :column current-col))
            heads)))
 (defun hydra--sort-heads (normalized-heads)
  "Return a list of non-nil doc heads grouped by identical property column
each heads of NORMALIZED-HEADS must have a column property"
  (let* ((heads-wo-nil-doc (cl-remove-if-not (lambda (head) (nth 2 head)) normalized-heads))
         (heads-sorted (cl-sort heads-wo-nil-doc (lambda (it other) (string< (hydra--head-property it :column)
                                                                             (hydra--head-property other :column))))))
    ;; this operation partition the sorted head list into lists of heads with same column property
    (cl-loop for head in heads-sorted
             for column-name = (hydra--head-property head :column)
             with prev-column-name = (hydra--head-property (nth 0 heads-sorted) :column)
             do (message "prev = %s  current = %s" prev-column-name column-name )
             unless (equal prev-column-name column-name) collect heads-one-column into heads-all-columns and do (setq heads-one-column nil)
             collect head into heads-one-column
             do (setq prev-column-name column-name)
             finally return (append heads-all-columns (list heads-one-column)))))

(defun hydra--pad-heads (heads-groups padding-head)
  "return a list of heads copied from HEADS-GROUPS where each heads group have the same length.
This is achieved by adding PADDING-HEAD were applicable"
  (cl-loop for head-group in heads-groups
           for this-head-group-length = (length head-group)
           with head-group-max-length = (apply #'max (mapcar (lambda (heads) (length heads)) heads-groups))
           if (<= this-head-group-length head-group-max-length)
           collect (append head-group (make-list (- head-group-max-length this-head-group-length) padding-head)) into balanced-head-groups
           else collect head-group into balanced-head-groups
           finally return balanced-head-groups))

(defun hydra--generate-matrix (heads-grouped-by-col)
  "Return a list of heads with even length from HEADS-GROUPED-BY-COL with padding heads where applicable and 2 virtual heads acting as table header
a property max-key-len and max-doc-len is also applied to each head and represents the maximum dimensions of the column"
  (when heads-grouped-by-col
    (cl-loop for heads in (hydra--pad-heads heads-grouped-by-col '(" " nil " " :exit t))
             for column-name = (hydra--head-property (nth 0 heads) :column)
             for max-key-len = (apply #'max (mapcar (lambda (x) (length (car x))) heads))
             for max-doc-len = (apply #'max (length column-name) (mapcar (lambda (x) (length (hydra--to-string (nth 2 x)))) heads))
             for header-virtual-head = `(" " nil ,column-name :column ,column-name :exit t)
             for separator-virtual-head = `(" " nil ,(make-string (+ 1 max-doc-len max-key-len) ?-) :column ,column-name :exit t)
             for decorated-heads = (copy-tree (apply 'list header-virtual-head separator-virtual-head heads))
             collect (mapcar (lambda (it)
                               (hydra--head-set-property it :max-key-len max-key-len)
                               (hydra--head-set-property it :max-doc-len max-doc-len))
                             decorated-heads)
             into decorated-heads-matrix
             finally return decorated-heads-matrix)))

(defun hydra--hint-from-matrix (body heads-matrix)
  "generate a formated doc string according to HEADS-MATRIX data and structure
HEADS-MATRIX is expected to be a list of heads 
Each heads must have the same length 
Each head must have a property max-key-len and max-doc-len
"
  (when heads-matrix
    (cl-loop with first-heads-col = (nth 0 heads-matrix)
             with last-row-index = (- (length first-heads-col) 1)
             for row-index from 0 to last-row-index
             for heads-in-row = (mapcar (lambda (heads) (nth row-index heads)) heads-matrix)
             concat (concat
                     (mapconcat (lambda (head)
                                  (funcall hydra-key-doc-function
                                           (hydra-fontify-head head body) ;; key
                                           (hydra--head-property head :max-key-len)
                                           (nth 2 head) ;; doc
                                           (hydra--head-property head :max-doc-len)))
                                heads-in-row "| ") "\n")
             into matrix-image
             finally return matrix-image)))

(defun mugu/hydra--hint (original-hydra-hint body heads)
  (let* ((sorted-heads (hydra--sort-heads (hydra--normalize-heads heads)))
         (heads-w-col (cl-remove-if-not (lambda (heads) (hydra--head-property (nth 0 heads) :column)) sorted-heads))
         (heads-wo-col (cl-remove-if (lambda (heads) (hydra--head-property (nth 0 heads) :column)) sorted-heads)))
    ;; (message "mugu original %s" heads)
    ;; (message "mugu unsorted %s" (hydra--normalize-heads heads))
    (concat "\n"
            (hydra--hint-from-matrix body (hydra--generate-matrix heads-w-col))
            (cond (heads-wo-col "\n" (funcall original-hydra-hint body (car heads-wo-col)))
                  (t "")))))

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
