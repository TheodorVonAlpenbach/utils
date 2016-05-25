(require 'ld-global)
(require 'ld-general)
(require 'ld-metadata)

(defun ld-sort-predicate (order column-definition)
  "ORDER is either :desc or :asc"
  (let ((order-pair (case (ld-column-type column-definition)
		      (string (list #'string> #'string<))
		      (t (list #'> #'<)))))
    (ecase order
      ((:desc :descending) (first order-pair))
      ((:asc :ascending nil) (second order-pair)))))

(defun default-identifier-scope (column-keyword schema verify-column-p)
  "Returns the column in SCHEMA that matches COLUMN-KEYWORD. If
VERIFY-COLUMN-P is true, it also checks for ambiguity."
  (cl-flet ((exists-p (keyword coldefs)
		      (cl-member keyword coldefs :key #'ld-column-keyword))) 
    (aif (exists-p column-keyword (ld-schema-column-definitions schema))
      (if (and verify-column-p
	       (exists-p column-keyword (rest it)))
	(error "Ambiguous column keyword")
	(list (ld-schema-identifier schema) column-keyword))
      (error "No matching column found in schema"))))
;;(default-identifier-scope '(:company :id) (ld-table-schema emps) t)
;;(mapcar #'ld-column-keyword (ld-table-column-definitions emps))

(cl-defun ld-schema-find-column-definition (identifier schema &key global-identifier-p verify-p)
  (if global-identifier-p
    (find identifier (ld-schema-column-definitions schema) :test #'equal :key #'ld-column-identifier)
    (ld-schema-find-column-definition (default-identifier-scope (ld-identifier-keyword identifier) schema verify-p) schema
				      :global-identifier-p t)))
;;(ld-schema-find-column-definition :id (ld-table-schema emps))

(cl-defun ld-table-find-column-definition (identifier table &rest args)
  (apply #'ld-schema-find-column-definition identifier (ld-table-schema table) args))
;;(ld-table-find-column-definition '(:employee :id) emps :global-identifier-p t)

(cl-defun globalize-column-identifier (identifier schema &key global-identifier-p allow-single-keyword-p verify-p)
  (if (listp identifier) 
    (case (length identifier)
      (0 (error "column identifier cannot be an empty list"))
      (1 (default-identifier-scope (first identifier) schema verify-p))
      (2 (aif (ld-schema-find-column-definition identifier schema :global-identifier-p global-identifier-p :verify-p verify-p)
	   (ld-column-identifier it)
	   (error "Could not find a matching column definition in %S for indentifier %S" 
		  (ld-schema-identifier schema)
		  identifier)))
      (t (error "Too many elements in column identifier")))
    (if allow-single-keyword-p
      (default-identifier-scope identifier schema verify-p)
      (error "single keyword not allowed"))))
;;(globalize-column-identifier :id (ld-table-schema emps) :allow-single-keyword-p t)
;;(ld-example-0)
;;(globalize-column-identifier '(:employee :id) (ld-table-schema (ld-join `(,emps :company-id) `(,comps :id))) :global-identifier-p t)


;;; Joined table
(cl-defun ld-mapper-1 (table column-identifier &rest args)
  "Returns a function that takes a row and returns the value or
  values at the specified slot or slots."
  (lexical-let ((pos (apply #'ld-column-position column-identifier (ld-table-schema table) args)))
    #'(lambda (row) (nth pos row))))
;;(ld-join `(,emps :company-id) `(,comps :id))
;;(ld-example-0)

(cl-defun ld-mapper (table column-identifiers &rest args)
  "Returns a function that takes a row and returns the value or
  values at the specified slot or slots."
  (if (atom column-identifiers) 
    (apply #'ld-mapper-1 table column-identifiers args)
    (error "not implemented")))


;;;; Operations

;;; Insert
(defun ld-check-row (row schema)
  (let ((coldefs (ld-schema-column-definitions schema))) 
    (and (= (length row) (length coldefs))
	 (every #'ld-check-type row coldefs))))

(defun ld-check-rows (rows schema)
  (every (bind #'ld-check-row schema) rows))

;;; insert
(defun ld-insert-row-list (table rows)
  (when *ld-check-data-p*
    (unless (ld-check-rows rows (ld-table-schema table))
      (error "Bad data")))
  (push-list rows (ld-table-data table)))

(defun ld-insert-row (table &rest rows)
  (ld-insert-row-list rows))

(cl-defun ld-column-position (column-designator &optional schema-designator)
  (cond ((integerp column-designator) column-designator)
	((ld-identifier-p column-designator :column)
	 (position column-designator 
		   (ld-schema-column-definitions (ld-schema (or schema-designator column-designator)))
		   :key #'ld-column-identifier
		   :test #'equal))
	((ld-column-p column-designator)
	 (ld-column-position (ld-column-identifier column-designator)
			     schema-designator))
	(t (error "Argument %S is not a column-designator" column-designator))))
;;(ld-column-position '(:maths :tasks :id) :tasks)

(cl-defun ld-row-value (row column-designator)
  (elt row (ld-column-position column-designator)))

(cl-defun ld-autogenerate (coldef &optional (table (ld-table coldef)))
  (aif (first (ld-table-data table))
    (+ (ld-row-value it coldef) *ld-autogenerate-increment*)
    *ld-autogenerate-base*))
;;(ld-autogenerate (first (ld-table-column-definitions (ld-table :users))))

(cl-defun ld-value-exists-p (value table coldef)
  "Checks if VALUE is present in column defined by COLDEF in TABLE"
  (find value (mapcar (bind #'elt (ld-column-position coldef table)) 
		      (ld-table-data table))
	:test (ld-column-comparator coldef)))

(cl-defun ld-violating-uniqueness-p (value table coldef)
  "Returns non-nil (in fact a message) iff COLDEF has property
:unique and VALUE is present in the TABLE's column that
correpsonds to coldef"
  (when (and (ld-column-property-p :unique coldef)
	     (ld-value-exists-p value table coldef))
    (ld-warning "Value %S is already present in table" value)))

(cl-defun ld-compile-new-row (table values columns)
  "This function only works with full rows.
It autogenerates values if this is specified and checks for the property unique"
  (let ((specfied-coldefs (ld-column-designators->coldefs columns table)))

    (assert (notany (bind #'ld-column-property-p :autogenerate 1) specfied-coldefs)
	    t "You cannot specify columns that are set to be autogenerated")

    (loop for coldef in (ld-table-column-definitions table)
	for autogenerated-p = (ld-column-property-p :autogenerate coldef)
	for value = (if autogenerated-p 
		      (ld-autogenerate coldef table)
		      (when (or (null columns) 
				(member* (ld-column-identifier coldef) specfied-coldefs
					:key #'ld-column-identifier
					:test #'equal))
			(pop values)))
	;; check for uniqueness
	if (ld-violating-uniqueness-p value table coldef) return nil
	collect value)))
;;(ld-compile-new-row (ld-table :users) '("Ludvik" "7" 1500 350) nil)

(cl-defun ld-create-metadata (&optional (dttm (iso-date-and-time)))
  (list :metadata :created dttm :updated dttm))

(cl-defun ld-insert (tab values &key columns)
  (let ((table (ld-table tab)))
    (awhen (ld-compile-new-row table values columns)
      (let ((row (append it (list (ld-create-metadata)))))
	(push row (ld-table-data table))
	row))))
;;(ld-insert :users '("Mats" "42" 1500 350))
;;(ld-insert :users '("Ludvik" "7" 1500 350))
;;(ld-table-data (ld-table :users))

;;; Join
(defun ld-join (table-column1 table-column2)
  "Joins tables"
  (cl-flet ((tabsort (tabcol)
	       (destructuring-bind (tab col) tabcol
		 (ld-create-table (ld-table-schema tab) 
				  (cl-sort (copy-list (ld-table-data tab))
					   #'< :key (ld-mapper tab col :allow-single-keyword-p t))))))
    (let* ((tabs (mapcar #'tabsort (list table-column1 table-column2)))
	   (tab1 (first tabs))
	   (tab2 (second tabs))
	   (rows1 (ld-table-data tab1))
	   (rows2 (ld-table-data tab2))
	   (mapper1 (ld-mapper tab1 (second table-column1) :allow-single-keyword-p t))
	   (mapper2 (ld-mapper tab2 (second table-column2) :allow-single-keyword-p t))
	   (empty-row1 (make-list (length (ld-table-column-definitions tab1)) nil))
	   (empty-row2 (make-list (length (ld-table-column-definitions tab2)) nil))
	   res)
      (while (or rows1 rows2)
	(let ((f1 (and rows1 (funcall mapper1 (first rows1))))
	      (f2 (and rows2 (funcall mapper2 (first rows2)))))
	  (if (or (and f1 (not f2)) (< f1 f2))
	    (push (append (pop rows1) nil) res)
	    (if (or (and (not f1) f2) (> f1 f2))
	      (push (append nil (pop rows2)) res)
	      ;; else they are equal
	      (progn 
		(push (append (first rows1) (first rows2)) res)
		(let ((s1 (funcall mapper1 (second rows1)))
		      (s2 (funcall mapper2 (second rows2))))
		  (if (and s1 (= f1 s1) (or (not s2) (/= f2 s2)))
		    (pop rows1)
		    (if (and s1 (/= f1 s1) s2 (= f2 s2))
		      (pop rows2)
		      (progn (pop rows1) (pop rows2))))))))))
      (ld-create-table (apply #'ld-join-schemas (mapcar #'ld-table-schema tabs)) (nreverse res)))))
;;(ld-join `(,emps :company-id) `(,comps :id))

(defun ld-joined-p (table)
  (eql (first table) :joined))

(defun ld-metadata-identifier-p (form)
  "Metadata column identifier starts with ::, e.g. ::created"
  (and (keywordp form)
       (char= (char (symbol-name form) 1) ?:)))

(defun ld-metadata-identifier (form)
  "Metadata column identifier starts with ::, e.g. ::created
Returns nil iif form is not a metadata identifier"
  (and (ld-metadata-identifier-p form)
       (intern-soft (substring (symbol-name form) 1))))
;;(ld-metadata-identifier ::created)

;;; Expressions
(defun ld-expression-rec (form schema row)
  (unless (ld-schema-p schema)
    (error "Schema is not valid: form = %S, schema = %S, row = %S" form schema row))
  (cond 
    ((ld-column-identifier-p form schema)
     `(nth ,(ld-column-position (ld-make-column-identifier form schema) schema) ,row))
    ((ld-metadata-identifier-p form)
     `(ld-get-metadatum ,(ld-metadata-identifier form) (ld-row-metadata ,row))) 
    ((atom form) form)
    (t (mapcar #'(lambda (x) (ld-expression-rec x schema row)) form))))
;;(ld-expression (= ::created 1) :users)

(defmacro ld-expression (expression schema)
  "Better naming"
  (with-gensyms (gx)
    `(lambda (,gx) ,(ld-expression-rec expression (ld-schema schema) gx))))
;;(ld-expression (= :id 1) :users)
;;(mapcar (ld-expression :test :users) (ld-table-data (ld-table :users)))
;;(ld-select :users)
;;(ld-expression (string= (:name) "Mats") :users)

(defmacro ld-expressions (expressions schema)
  ""
  `(list (ld-expression ,(first expressions) ,schema)
	 (ld-expression ,(second expressions) ,schema)))

(defmacro ld-expressions (expressions schema)
  ""
  `(list ,@(mapcar #'(lambda (x) `(ld-expression ,x ,schema)) expressions)))
;;(ld-expressions ((+ 1 (:id))) :users)
;;(mapcar (first (ld-expressions ((+ 1 (:id))) :users)) (ld-table-data (ld-table :users)))
;;(ld-expressions nil :users)
;;(ld-expressions (:rating :name) :users)
;;(every #'functionp (ld-expressions (:rating :name) :users))

(defun ld-colexp (form schema-designator)
  (ld-column-position (ld-make-column-identifier form schema-designator) schema-designator))

(defmacro ld-column-expression->number (form schema-designator list-p)
  (when form
    (if list-p
      `(list ,@(mapcar (bind #'ld-colexp schema-designator) form))
    (ld-colexp form schema-designator))))
;;(ld-column-expression->number ((:id) (:id)) :users t)
;;(ld-column-expression->number (:maths :users :id) :users nil)

;;;; Select
(cl-defun ld-select-1 (table &key where columns column order order-by copy format)
  "Helper function for `ld-select'"
  (let* ((rows (copy-if (or where #'always) (ld-table-data table))))
    (cl-flet ((colpos (column) (ld-column-position column table-designator)))
      (when (or order order-by)
	(let* ((schema (ld-table-schema table)))
	  (setf rows (sort* (copy-list rows)
			    (ld-sort-predicate order (nth order-by (ld-schema-column-definitions schema)))
			    :key (lexical-let ((col (or order-by (ld-colexp (ld-primary-key (ld-schema schema)) schema))))
				   #'(lambda (row) (elt row col)))))))
      (when columns
        (setf rows (apply #'project-sequence rows columns)))
      (when column
	(if columns
	  (ld-warning "Both keyword argument :columns and :column specified. Discards the latter.")
	  (setf rows (mapcar #'car (project-sequence rows column)))))
      (if format
	(concat* rows :key #'(lambda (x) (apply #'format format x)))
	(if copy (copy-tree rows) rows)))))
;;(ld-select-1 (ld-table :users))

(cl-defmacro ld-select (table-designator &rest args)
  "For :where and :column(s) we must convert the following expression by substituting column expressions with row extracting lambdas.
For order-by we must convert columns to column-positions."
  `(ld-select-1 (ld-table ,table-designator)
		:where (and ',(getf args :where) (ld-expression ,(popf args :where) ,table-designator))
		:columns (ld-expressions ,(popf args :columns) ,table-designator)
		:column (and ',(getf args :column) (ld-expression ,(popf args :column) ,table-designator))
		:order-by (and ',(getf args :order-by) (ld-column-expression->number ,(popf args :order-by) ,table-designator nil))
		,@args))
;;(macroexpand '(ld-select :users))
;;(ld-select :users :columns ((funcall (lambda (x) (* 2 x)) (:id)) (:name)) :format "doubled id is %s, and name his name shall be called %s")
;;(ld-select :users :columns (:rating))
;;(macroexpand '(ld-select :users :columns (:rating)))


;;; Delete
(defun ld-delete-if (predicate table-designator)
  (let ((table (ld-table table-designator)))
    (setf (ld-table-data table)
	  (delete-if predicate (ld-table-data table)))))

(cl-defmacro ld-delete (table-designator where)
  `(ld-delete-if (ld-expression ,where ,table-designator) ,table-designator))
;;(pp (ld-delete :users (= (:id) 1)))

;;; Update
(defun ld-update-1 (table where-function values columns)
  "Skipping properties for now"
  (assert (= (length (llist values)) (length (llist columns)))
	  t "The number of values and the number columns are not the same")
  (let ((rows (copy-if where-function (ld-table-data table))))
    (case (length rows)
      (0 (ld-warning "No single row was selected in :where expression."))
      (1 (let ((row (first rows)))
	   (ld-set-metadatum (iso-date-and-time) :updated (ld-row-metadata (first rows)))
	   (loop for value in (llist values)
		 for colfun in (llist columns)
		 do (setf (nth colfun row) value)
		 finally return row)))
      (t (ld-warning "Skipping update since multiple rows were selected in :where expression.")))))

(defmacro ld-update (table-designator where vals columns)
  "For some very strange reason, this macro fails if VALS is renamed to VALUES.
No, if fact look up variable `values'. This is defined in core
Emacs! So any Emacs macro should avoid using this symbol.
But still I really don't understand the behaviour of values in a macro."
  `(ld-update-1 ',(ld-table table-designator)
		(ld-expression ,where ,table-designator)
		,vals
		(ld-column-expression->number ,columns ,table-designator (listp ,vals))))
;;(ld-update :users (= :id 3) "Mats" (:name))
;;(ld-update :users (= :id 3) 1892.8716637254522 (:rating))
;;(ld-select :users :where (string= :name "Mats") :column :rating)

(provide 'ld-operations)
