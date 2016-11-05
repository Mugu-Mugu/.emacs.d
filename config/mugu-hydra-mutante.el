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
  "Return a list of heads grouped by identical property column UNSORTED-HEADS"
  (--partition-by (plist-get it :column)
                  (--sort (string< (plist-get it :column) (plist-get other :column)) unsorted-heads)))

(defun hydra--generate-matrix (heads-grouped-by-col)
  "Return a square matrix of heads (with padding if needed) and add to each head 2 properties max key/doc size
from HEADS-GROUPED-BY-COL (a list of heads)"
  (-map (lambda (heads) "compute the max key/doc size for this heads list and apply it to each head of this group"
          (message "heads : %s" heads)
          (let* ((column-name (plist-get (-first-item heads) :column))
                 (heads-with-header (-insert-at 0 `(" " nil ,(concat "[" column-name "]") nil :column ,column-name :exit t) heads))
                 (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) heads-with-header)))
                 (max-doc-len (apply #'max (mapcar (lambda (x) (length (hydra--to-string (nth 2 x)))) heads-with-header))))
            (message "heads-with-header : %s" heads-with-header)
            (--map (progn (plist-put it :max-key-len max-key-len)
                          (plist-put it :max-doc-len max-doc-len))
                   heads-with-header)))
        (-clone (apply '-pad '(" " nil "" nil :exit true) heads-grouped-by-col))))

(defun hydra--hint-from-matrix (body heads-matrix)
  "generate a formated doc string according to HEADS-MATRIX data and structure"
  (message "%s" heads-matrix)
  (mapconcat (lambda (heads)
               (message "%s" heads)
               (mapconcat (lambda (it)
                            (funcall hydra-key-doc-function
                                     (hydra-fontify-head it body) ;; key
                                     (plist-get it :max-key-len)
                                     (nth 2 it) ;; doc
                                     (plist-get it :max-doc-len)))
                          heads
                          "| "))
             (-partition-all (length heads-matrix) (apply '-interleave heads-matrix)) "\n"))

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

      ;; normalize
      ;;  > step 1 = add a fake args so the list is even)
      ;;  > step 2 = ensure all head have a column or default it to nil
      ;;  > step 3 = sort by column name
      ;;  >> > step 4 = filter by empty hint
      ;; generate matrix
      ;;  >> > step 1 = (optional : exclude the one without column)
      ;;  > step 2 = partition list by column name
      ;;  > step 3 = pad the matrix with default head
      ;;  > step 4 = for each column compute the maximum size length and populate it
      ;;  > step 5 = interspece the matrix
      (setq res
            (if n-cols
                (let ((n-rows (1+ (/ (length keys) n-cols)))
                      (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) keys)))
                      (max-doc-len (apply #'max (mapcar (lambda (x) (length (hydra--to-string (cdr x)))) keys))))
                  `(concat
                    "\n"
                    (mapconcat #'identity
                               (mapcar
                                (lambda (x)
                                  (mapconcat
                                   (lambda (y)
                                     (and y
                                          (funcall hydra-key-doc-function
                                                   (concat "_" (car y) " ")
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
        res)))
  (message "%s" (hydra--hint-from-matrix body (hydra--generate-matrix (hydra--sort-heads (hydra--normalize-heads heads)))))
  (hydra--hint-from-matrix body (hydra--generate-matrix (hydra--sort-heads (hydra--normalize-heads heads)))))


(defun mugu/hydra-fontify (head body)
  "Produce a pretty string from HEAD and BODY.
HEAD's binding is returned as a string with a colored face."
  (message "fontify %s" head)
  (if (eq " " (nth 0 head))
      " "
    (let* ((foreign-keys (hydra--body-foreign-keys body))
           (head-exit (plist-get head :exit))
           (head-color
            (if head-exit
                (if (eq foreign-keys 'warn)
                    'teal
                  'blue)
              (cl-case foreign-keys
                (warn 'amaranth)
                (run 'pink)
                (t 'red)))))
      (when (and (null (cadr head))
                 (not head-exit))
        (hydra--complain "nil cmd can only be blue"))
      (propertize (if (string= (car head) "%")
                      "%%"
                    (car head))
                  'face
                  (or (plist-get head :face)
                      (cl-case head-color
                        (blue 'hydra-face-blue)
                        (red 'hydra-face-red)
                        (amaranth 'hydra-face-amaranth)
                        (pink 'hydra-face-pink)
                        (teal 'hydra-face-teal)
                        (t (error "Unknown color for %S" head))))))))

(defun mugu/hydra-doc-format (key key-width doc doc-width)
  "Doc"
  (if (equal key " ")
      (format (format "%%-%ds   " (+ key-width doc-width)) doc)
    (format (format "%%%ds: %%%ds" key-width (- -1 doc-width))
            key doc)))

(setq hydra-fontify-head-function 'mugu/hydra-fontify)
(setq hydra-key-doc-function 'mugu/hydra-doc-format)


(defhydra mugu-menu-help-hydra (:color blue :hint nil)
  "
MUGU
"
  ("mr" helm-man-woman "muug")
  (" " nil "[sous-mugu]")
  ("mz" helm-apropos "gros mugu z")
  ("i" helm-info-find "ezfezfezefzef" :column "mugu2")
  ("a" helm-apropos "gros mugu super long" )
  ("fiz" helm-info-find "dfiz" )
  ("fa" helm-apropos "gros fa super long" )
  ("h" helm-documentation "mgu":column "mugu3"))
(call-interactively 'mugu-menu-help-hydra/body)
