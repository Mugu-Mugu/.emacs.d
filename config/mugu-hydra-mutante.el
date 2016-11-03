
(defun hydra--normalize-heads (unormalized-heads)
  "transform UNORMALIZED-HEADS with 2 actions : 
-> add a padding value in each head in order for them be interpretable as a plist 
-> ensure all head have a column property"
  (let ((current-col nil)) (-map (lambda (head)
                                   (if (plist-member head :column)
                                       (setq current-col (plist-get head :column)))
                                   (plist-put head :column current-col))
                                 (--map (-insert-at 3 nil it) unormalized-heads))))

(defun hydra--sort-heads (unsorted-heads)
  "Return a sorted list grouped by head with similar property column from UNSORTED-HEAD"
  (--sort (string< (plist-get it :column) (plist-get other :column)) unsorted-heads))

(defun hydra--hint (body heads)
  "Generate a hint for the echo area.
 BODY, and HEADS are parameters to `defhydra'."
  (let (alist)
    (dolist (h heads)
      (let ((val (assoc (cadr h) alist))
            (pstr (hydra-fontify-head h body)))
        (unless (null (cl-caddr h))
          (if val
              (setf (cadr val)
                    (concat (cadr val) " " pstr))
            (push
             (cons (cadr h)
                   (cons pstr (cl-caddr h)))
             alist)))))
    (let ((keys (nreverse (mapcar #'cdr alist)))
          (n-cols (plist-get (cddr body) :columns))
          res)
      (let ((column-indices (--find-indices (plist-member (-drop 3 it) :column) heads)))
        (message "indices is %s" column-indices))
      (--map (message "%s" (-drop 3 it)) heads)

      ;; normalize
      ;;  > step 1 = add a fake args so the list is even)
      ;;  > step 2 = ensure all head have a column or default it to nil
      ;;  > step 3 = sort by column name
      ;;  > step 4 = filter by empty hint
      ;; generate matrix
      ;;  > step 1 = (optional : exclude the one without column)
      ;;  > step 2 = partition list by column name
      ;;  > step 3 = pad the matrix with default head
      ;;  > step 4 = for each column compute the maximum size length and populate it
      ;;  > step 5 = interspece the matrix
      (message "normalized : %s" (hydra--normalize-heads heads))
      (message "sorted and normalized : %s" (hydra--sort-heads (hydra--normalize-heads heads)))
      (message "partitioned and sorted : %s" (--partition-by (plist-get it :column) (hydra--sort-heads (hydra--normalize-heads heads))))
      (setq res
            (if n-cols
                (let ((n-rows (1+ (/ (length keys) n-cols)))
                      (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) keys)))
                      (max-doc-len (apply #'max (mapcar (lambda (x)
                                                          (length (hydra--to-string (cdr x)))) keys))))
                  `(concat
                    "\n"
                    (mapconcat #'identity
                               (mapcar
                                (lambda (x)
                                  (mapconcat
                                   (lambda (y)
                                     (and y
                                          (funcall hydra-key-doc-function
                                                   (car y)
                                                   ,max-key-len
                                                   (hydra--to-string (cdr y))
                                                   ,max-doc-len))) x ""))
                                ',(hydra--matrix keys n-cols n-rows))
                               "\n")))


              `(concat
                (mapconcat
                 (lambda (x)
                   (let ((str (hydra--to-string (cdr x))))
                     (format
                      (if (> (length str) 0)
                          (concat hydra-head-format str)
                        "%s")
                      (car x))))
                 ',keys
                 ", ")
                ,(if keys "." ""))))
      (if (cl-every #'stringp
                    (mapcar 'cddr alist))
          (eval res)
        res))))
(defhydra mugu-menu-help-hydra (:color teal
                                       :hint nil)
  "
^EMACS^             ^Helm^         
^^^^^^^^---------------------------
_m_: man           _h_: helm
_i_: info          ^ ^
_a_: apropos       ^ ^
"
  ("m" helm-man-woman "muug" :column "mugu")
  ("i" helm-info-find :column "mugu2")
  ("a" helm-apropos :column "mugu3")
  ("h" helm-documentation "mgu" :column "mugu2"))
