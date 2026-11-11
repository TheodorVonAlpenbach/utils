;;; ld-operations.el --- Operations on list databases -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ld-global)
(require 'ld-general)
(require 'ld-metadata)
(require 'ld-utils)

(cl-defun ld-sort-predicate (order column-definition)
  "ORDER is either :desc or :asc"
 (let ((order-pair (cl-case (ld-column-type column-definition)
		      (string (list #'string> #'string<))
		      (t (list #'> #'<)))))
    (cl-ecase order
      ((:desc :descending) (car order-pair))
      ((:asc :ascending nil) (cadr order-pair)))))

(cl-defun default-identifier-scope (column-keyword schema verify-column-p)
  "Returns the column in SCHEMA that matches COLUMN-KEYWORD. If
VERIFY-COLUMN-P is true, it also checks for ambiguity."
  (cl-flet ((exists-p (keyword coldefs)
		      (cl-member keyword coldefs :key #'ld-column-keyword))) 
    (let ((matches
           (exists-p
            column-keyword
            (ld-schema-column-definitions schema))))
      (if matches
          (if (and verify-column-p
                   (exists-p column-keyword (cdr matches)))
              (user-error "Ambiguous column keyword: %S" column-keyword)
            (list (ld-schema-identifier schema) column-keyword))
        (user-error "No matching column found in schema: %S"
                    column-keyword)))))
;;(default-identifier-scope '(:company :id) (ld-table-schema emps) t)
;;(mapcar #'ld-column-keyword (ld-table-column-definitions emps))

(cl-defun ld-schema-find-column-definition (identifier schema &key global-identifier-p verify-p)
  (if global-identifier-p
    (cl-find identifier (ld-schema-column-definitions schema) :test #'equal :key #'ld-column-identifier)
    (ld-schema-find-column-definition (default-identifier-scope (ld-identifier-keyword identifier) schema verify-p) schema
				      :global-identifier-p t)))
;;(ld-schema-find-column-definition :id (ld-table-schema emps))

(cl-defun ld-table-find-column-definition (identifier table &rest args)
  (apply #'ld-schema-find-column-definition identifier (ld-table-schema table) args))
;;(ld-table-find-column-definition '(:employee :id) emps :global-identifier-p t)

(cl-defun globalize-column-identifier (identifier schema &key global-identifier-p allow-single-keyword-p verify-p)
  (if (listp identifier) 
    (cl-case (length identifier)
      (0 (error "column identifier cannot be an empty list"))
      (1 (default-identifier-scope (car identifier) schema verify-p))
      (2 (let ((column
                (ld-schema-find-column-definition
                 identifier schema
                 :global-identifier-p global-identifier-p
                 :verify-p verify-p)))
           (if column
               (ld-column-identifier column)
             (user-error
              "Could not find a matching column in %S for identifier %S"
              (ld-schema-identifier schema)
              identifier))))
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
  (let ((pos (apply #'ld-column-position column-identifier (ld-table-schema table) args)))
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
  "Return non-nil when ROW conforms to SCHEMA."
  (let ((coldefs (ld-schema-column-definitions schema)))
    (and (= (length row) (length coldefs))
	 (cl-every #'ld-check-type row coldefs))))

(defun ld-check-rows (rows schema)
  "Return non-nil when every row in ROWS conforms to SCHEMA."
  (cl-every (lambda (row) (ld-check-row row schema)) rows))

;;; insert
(defun ld-insert-row-list (table rows)
  "Prepend ROWS to TABLE's data and return the resulting data."
  (when *ld-check-data-p*
    (unless (ld-check-rows rows (ld-table-schema table))
      (user-error "Rows do not conform to the table schema")))
  (setf (ld-table-data table)
        (append rows (ld-table-data table))))

(defun ld-insert-row (table &rest rows)
  "Prepend ROWS to TABLE's data."
  (ld-insert-row-list table rows))

(defun ld-column-position (column-designator &optional schema-designator)
  "Return COLUMN-DESIGNATOR's zero-based position.
Resolve the column in SCHEMA-DESIGNATOR when supplied.  Return nil
when a valid identifier does not occur in the schema."
  (cond
   ((integerp column-designator)
    column-designator)
   ((ld-column-p column-designator)
    (ld-column-position
     (ld-column-identifier column-designator)
     (or schema-designator
         (ld-column-identifier column-designator))))
   ((ld-identifier-p column-designator :column)
    (when-let ((schema
                (ld-schema (or schema-designator column-designator))))
      (cl-position
       column-designator
       (ld-schema-column-definitions schema)
       :key #'ld-column-identifier
       :test #'equal)))
   (t
    (user-error "Not a column designator: %S" column-designator))))
;;(ld-column-position '(:maths :tasks :id) :tasks)

(defun ld-row-value (row column-designator &optional schema-designator)
  "Return ROW's value for COLUMN-DESIGNATOR."
  (let ((position
         (ld-column-position column-designator schema-designator)))
    (unless position
      (user-error "Column is not present in the schema: %S"
                  column-designator))
    (elt row position)))

(defun ld-autogenerate (column-definition &optional table)
  "Return the next automatic value for COLUMN-DEFINITION in TABLE."
  (let ((target (or table (ld-table column-definition))))
    (if-let ((row (car (ld-table-data target))))
        (+ (ld-row-value row column-definition target)
           *ld-autogenerate-increment*)
      *ld-autogenerate-base*)))
;;(ld-autogenerate (first (ld-table-column-definitions (ld-table :users))))

(defun ld-value-exists-p (value table column-definition)
  "Return non-nil when VALUE occurs in COLUMN-DEFINITION of TABLE."
  (let ((position (ld-column-position column-definition table))
        (test (ld-column-comparator column-definition)))
    (cl-some
     (lambda (row)
       (funcall test value (elt row position)))
     (ld-table-data table))))

(defun ld-violating-uniqueness-p (value table column-definition)
  "Return non-nil when VALUE violates COLUMN-DEFINITION in TABLE."
  (and (ld-column-property-p :unique column-definition)
       (ld-value-exists-p value table column-definition)))

(defun ld-compile-new-row (table values columns)
  "Build a row for TABLE from VALUES assigned to COLUMNS.
Autogenerated columns must not be specified explicitly."
  (let* ((column-definitions (ld-table-column-definitions table))
         (specified
          (if columns
              (ld-column-designators->coldefs columns table)
            (cl-remove-if
             (lambda (column)
               (ld-column-property-p :autogenerate column))
             column-definitions))))
    (when (memq nil specified)
      (user-error "One or more columns do not exist"))
    (when (cl-some
           (lambda (column)
             (ld-column-property-p :autogenerate column))
           specified)
      (user-error "Autogenerated columns cannot be specified"))
    (unless (= (length values) (length specified))
      (user-error "Expected %d values, received %d"
                  (length specified) (length values)))
    (let ((assignments (cl-mapcar #'cons specified values)))
      (mapcar
       (lambda (column)
         (let ((value
                (if (ld-column-property-p :autogenerate column)
                    (ld-autogenerate column table)
                  (cdr (assq column assignments)))))
           (when (ld-violating-uniqueness-p value table column)
             (user-error "Value %S is already present in column %S"
                         value (ld-column-keyword column)))
           value))
       column-definitions))))
;;(ld-compile-new-row (ld-table :users) '("Ludvik" "7" 1500 350) nil)

(defun ld-create-metadata (&optional date-time)
  "Create row metadata using DATE-TIME or the current time."
  (let ((timestamp
         (or date-time
             (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    (list :metadata :created timestamp :updated timestamp)))

(cl-defun ld-insert (tab values &key columns)
  "Insert VALUES into TAB and return the new row.
COLUMNS optionally identifies the supplied values."
  (let ((table (ld-table tab)))
    (unless table
      (user-error "Table does not exist: %S" tab))
    (let ((row
           (append (ld-compile-new-row table values columns)
                   (list (ld-create-metadata)))))
      (push row (ld-table-data table))
      row)))
;;(ld-insert :users '("Mats" "42" 1500 350))
;;(ld-insert :users '("Ludvik" "7" 1500 350))
;;(ld-table-data (ld-table :users))

;;; Join
(cl-defun ld-join (table-column1 table-column2)
  "Joins tables"
  (cl-flet ((tabsort (tabcol)
	      (cl-destructuring-bind (tab col) tabcol
		(ld-create-table
		 (ld-table-schema tab) 
		 (cl-sort (copy-sequence (ld-table-data tab))
		   #'< :key (ld-mapper tab col :allow-single-keyword-p t))))))
    (let* ((tabs (mapcar #'tabsort (list table-column1 table-column2)))
	   (tab1 (car tabs))
	   (tab2 (cadr tabs))
	   (rows1 (ld-table-data tab1))
	   (rows2 (ld-table-data tab2))
	   (mapper1 (ld-mapper tab1 (cadr table-column1)
			       :allow-single-keyword-p t))
	   (mapper2 (ld-mapper tab2 (cadr table-column2)
			       :allow-single-keyword-p t))
	   res)
      (while (or rows1 rows2)
	(let ((f1 (and rows1 (funcall mapper1 (car rows1))))
	      (f2 (and rows2 (funcall mapper2 (car rows2)))))
	  (if (or (and f1 (not f2)) (< f1 f2))
	    (push (append (pop rows1) nil) res)
	    (if (or (and (not f1) f2) (> f1 f2))
	      (push (append nil (pop rows2)) res)
	      ;; else they are equal
	      (progn 
		(push (append (car rows1) (car rows2)) res)
		(let ((s1 (funcall mapper1 (cadr rows1)))
		      (s2 (funcall mapper2 (cadr rows2))))
		  (if (and s1 (= f1 s1) (or (not s2) (/= f2 s2)))
		    (pop rows1)
		    (if (and s1 (/= f1 s1) s2 (= f2 s2))
		      (pop rows2)
		      (progn (pop rows1) (pop rows2))))))))))
      (ld-create-table (apply #'ld-join-schemas (mapcar #'ld-table-schema tabs))
		       (nreverse res)))))
;;(ld-join `(,emps :company-id) `(,comps :id))

(cl-defun ld-joined-p (table)
  (eql (car table) :joined))

(cl-defun ld-metadata-identifier-p (form)
  "Metadata column identifier starts with ::, e.g. ::created"
  (when (keywordp form)
    (let ((name (symbol-name form)))
      (and (> (length name) 1)
           (= (aref name 1) ?:)))))

(cl-defun ld-metadata-identifier (form)
  "Metadata column identifier starts with ::, e.g. ::created
Returns nil iif form is not a metadata identifier"
  (and (ld-metadata-identifier-p form)
       (intern-soft (substring (symbol-name form) 1))))
;;(ld-metadata-identifier ::created)

;;; Expressions
(defun ld-expression-value (form schema-designator row)
  "Resolve FORM against SCHEMA-DESIGNATOR and ROW.
Column designators return their row value, metadata designators
return metadata, and unmatched keywords remain literal."
  (if (ld-metadata-identifier-p form)
      (ld-get-metadatum
       (ld-metadata-identifier form)
       (ld-row-metadata row))
    (let ((schema (ld-schema schema-designator)))
      (unless schema
        (user-error "Schema does not exist: %S" schema-designator))
      (if-let ((column (ld-find-column-definition form schema)))
          (ld-row-value row column schema)
        (if (keywordp form)
            form
          (user-error "Column does not exist: %S" form))))))

(defun ld-expression-rec (form schema-designator row)
  "Expand FORM into a row expression resolved at runtime.
SCHEMA-DESIGNATOR and ROW are unevaluated forms inserted into the
result."
  (cond
   ((or (keywordp form)
        (ld-identifier-p form))
    `(ld-expression-value
      ',form ,schema-designator ,row))
   ((atom form)
    form)
   ((eq (car form) 'quote)
    form)
   (t
    (mapcar
     (lambda (item)
       (ld-expression-rec item schema-designator row))
     form))))
;;(ld-expression (= ::created 1) :users)

(defmacro ld-expression (expression schema-designator)
  "Return a row predicate for EXPRESSION and SCHEMA-DESIGNATOR.
Column and metadata positions are resolved when the predicate is
called, not while this macro is expanded."
  (let ((row (cl-gensym "row")))
    `(lambda (,row)
       ,(ld-expression-rec expression schema-designator row))))
;;(ld-expression (= :id 1) :users)
;;(mapcar (ld-expression :test :users) (ld-table-data (ld-table :users)))
;;(ld-select :users)
;;(ld-expression (string= (:name) "Mats") :users)

(defmacro ld-expressions (expressions schema-designator)
  "Return row functions for EXPRESSIONS in SCHEMA-DESIGNATOR."
  `(list
    ,@(mapcar
       (lambda (expression)
         `(ld-expression ,expression ,schema-designator))
       expressions)))
;;(ld-expressions ((+ 1 (:id))) :users)
;;(mapcar (first (ld-expressions ((+ 1 (:id))) :users)) (ld-table-data (ld-table :users)))
;;(ld-expressions nil :users)
;;(ld-expressions (:rating :name) :users)
;;(cl-every #'functionp (ld-expressions (:rating :name) :users))

(defun ld-colexp (form schema-designator)
  "Return FORM's column position in SCHEMA-DESIGNATOR."
  (let* ((schema (ld-schema schema-designator))
         (identifier
          (and schema
               (ld-make-column-identifier form schema)))
         (position
          (and identifier
               (ld-column-position identifier schema))))
    (unless position
      (user-error "Column does not exist: %S" form))
    position))

(defun ld-column-expression->number (form schema-designator list-p)
  "Resolve FORM to column positions in SCHEMA-DESIGNATOR.
Return a list of positions when LIST-P is non-nil."
  (when form
    (if list-p
        (mapcar
         (lambda (column)
           (ld-colexp column schema-designator))
         form)
      (ld-colexp form schema-designator))))
;;(ld-column-expression->number ((:id) (:id)) :users t)
;;(ld-column-expression->number (:maths :users :id) :users nil)

;;;; Select
(cl-defun ld-select-1 (table &key where columns column order order-by copy format)
  "Helper function for `ld-select'"
  (unless table
    (user-error "Table does not exist"))
  (let ((rows
         (cl-remove-if-not
          (or where #'always)
          (ld-table-data table))))
    (when (or order order-by)
      (let* ((schema (ld-table-schema table))
             (position
              (or order-by
                  (ld-column-position (ld-primary-key schema) schema)))
             (column-definition
              (nth position
                   (ld-schema-column-definitions schema))))
        (unless column-definition
          (user-error "Cannot determine a column to sort by"))
        (setq rows
              (cl-sort
               (copy-sequence rows)
               (ld-sort-predicate order column-definition)
               :key (lambda (row) (elt row position))))))
    (when columns
      (setq rows
            (mapcar
             (lambda (row)
               (mapcar
                (lambda (expression)
                  (funcall expression row))
                columns))
             rows)))
    (when column
      (if columns
          (ld-warning
           "Both :columns and :column were specified; ignoring :column")
        (setq rows
              (mapcar
               (lambda (row) (funcall column row))
               rows))))
    (if format
        (mapconcat
         (lambda (row)
           (if (listp row)
               (apply #'format format row)
             (format format row)))
         rows "")
      (if copy (copy-tree rows) rows))))
;;(ld-select :users :columns (:name))
;;(ld-select-1 (ld-table :users))

(cl-defmacro ld-select (table-designator &rest args)
  "Select rows from the table specified by TABLE-DESIGNATOR.

Keywords supported"
  (let ((where (plist-get args :where))
        (columns (plist-get args :columns))
        (column (plist-get args :column))
        (order-by (plist-get args :order-by))
        (remaining
         (cl-loop for (key value) on args by #'cddr
                  unless (memq key
                               '(:where :columns :column :order-by))
                  append (list key value))))
    `(ld-select-1
      (ld-table ,table-designator)
      :where ,(when where
                `(ld-expression ,where ,table-designator))
      :columns (ld-expressions ,columns ,table-designator)
      :column ,(when column
                 `(ld-expression ,column ,table-designator))
      :order-by ,(when order-by
                   `(ld-column-expression->number
                     ',order-by ,table-designator nil))
      ,@remaining)))
(put 'ld-select 'lisp-indent-function 'prog1)
;;(macroexpand '(ld-select :users))
;;(ld-select :users :columns ((funcall (lambda (x) (* 2 x)) (:id)) (:name)) :format "doubled id is %s, and name his name shall be called %s")
;;(ld-select :users :columns (:rating))
;;(macroexpand '(ld-select :users :columns (:rating)))


;;; Delete
(cl-defun ld-delete-if (predicate table-designator)
  (let ((table (ld-table table-designator)))
    (setf (ld-table-data table)
	  (cl-delete-if predicate (ld-table-data table)))))

(cl-defmacro ld-delete (table-designator where)
  `(ld-delete-if (ld-expression ,where ,table-designator) ,table-designator))
;;(pp (ld-delete :users (= (:id) 1)))

;;; Update
(cl-defun ld-update-1 (table-designator where-function values columns)
  "Skipping properties for now"
  (let ((value-list (if (listp values) values (list values)))
        (column-list (if (listp columns) columns (list columns))))
  (cl-assert (= (length value-list) (length column-list))
	     t "The number of values and the number columns are not the same")
  (let* ((table (ld-table table-designator))
	 (rows (cl-remove-if-not where-function (ld-table-data table))))
    (cl-case (length rows)
      (0 (ld-warning "No single row was selected in :where expression."))
      (1 (let ((row (car rows)))
	   (ld-set-metadatum (format-time-string "%Y-%m-%dT%H:%M:%S%z")
			     :updated (ld-row-metadata (car rows)))
	   (cl-loop for value in value-list
		    for colfun in column-list
		    do (setf (nth colfun row) value)
		    do (message "Updated column no %d with %S" colfun value)
		    finally return row)))
      (t (ld-warning "Skipping update since multiple rows were selected in :where expression."))))))

(defmacro ld-update (table-designator where vals columns)
  "For some very strange reason, this macro fails if VALS is renamed to VALUES.
No, in fact, look up variable `values'. This is defined in core
Emacs! So any Emacs macro should avoid using this symbol.
But still I really don't understand the behaviour of values in a macro."
  ;; in this version, table-designator must be a keyword, e.g. :user
  ;; should make an expansion macro perhaps?
  `(ld-update-1 ,table-designator
		(ld-expression ,where ,table-designator)
		,vals
		(ld-column-expression->number
                 ',columns ,table-designator (listp ,vals))))
;;(ld-update :users (= :id 3) "Mats" (:name))
;;(ld-update :users (= :id 3) 1892.8716637254522 (:rating))
;;(ld-select :users :where (string= :name "Mats") :column :rating)

(provide 'ld-operations)

;;; ld-operations.el ends here
