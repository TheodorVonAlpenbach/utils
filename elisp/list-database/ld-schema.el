;;;; The table schema is a list on the following form
;;;; (:schema table-identifier table-name column-definition1 column-definition2 ... )

(require 'ld-identifier)
(require 'ld-column)

(defalias 'ld-schema-identifier #'second)
(defalias 'ld-schema-name #'third)
(defalias 'ld-schema-column-definitions #'fourth)

(defun ld-make-empty-schema (keyword name parent-keyword &optional raw-column-definitions)
  (let ((id (list parent-keyword keyword)))
    (assert (ld-identifier-p id))
    (list :schema id name raw-column-definitions)))
;;(make-employees)

(cl-defun ld-insert-column (schema keyword &key pos name type properties)
  "If TYPE is nil: no particular type. Otherwise use the type symbols in elisp."
  (list-insert (ld-make-column (append (ld-schema-identifier schema) (list keyword))
			       :name name :type type :properties properties)
	       (or pos (length (ld-schema-column-definitions schema)))
	       (ld-schema-column-definitions schema)))
;;(ld-create-database :qwe :schemas (mapcar (bind #'ld-schemadef->schema :qwe) *maths-db-schema-definitions*))

(cl-defun ld-add-columns (schema columns)
  "Each column in the list COLUMNS is a list with a mandatory
keyword as first elements. The rest of the list is a plist with
some subset of the properties :name :type :primary-key."
  (loop for col in columns do (apply #'ld-insert-column schema col)))

(cl-defun ld-make-schema (keyword &key (name (keyword-name keyword)) column-definitions (plist-p t) parent-keyword)
  "If PLIST-P is nil the COLUMN-DEFINITIONS are inserted into schema as is. Else, they are processed with"
  (if plist-p
    ;; first make an empty schema
    (let ((schema (ld-make-empty-schema keyword name parent-keyword)))
      ;; then add and process the columns specs
      (ld-add-columns schema column-definitions)
      schema)
    (ld-make-empty-schema keyword name parent-keyword column-definitions)))
;;(ld-make-schema :company :name "MB_COMPANY" :parent-keyword :mydb)

(defun ld-schemadef->schema (schemadef parent-keyword)
  "Short cut definition of a schema. See `*maths-db*' for an example.
The schema is set up like a skeleton of lists which resembles the
schema structure. This skeleton is here straightforward converted
to a legal scheme structure."
  (destructuring-bind (keyword name coldefs) schemadef
    (ld-make-schema keyword :name name :column-definitions coldefs :plist-p t :parent-keyword parent-keyword)))
;;(maths-make-schema (first *maths-db-schema-definitions*))
;;(ld-create-database :maths :schemas (mapcar #'ld-schemadef->schema *maths-db-schema-definitions*))

(cl-defun ld-join-schemas (&rest schemas)
  (and schemas
       (ld-make-schema
	(make-keyword (concat* (mapcar (compose #'downcase #'keyword-name #'ld-schema-identifier) schemas)
			       :in "-and-" :pre "join-of-"))
	:column-definitions (apply #'append (mapcar #'ld-column-definitions schemas))
	:plist-p nil)))
;;(ld-join-schemas (ld-schema emps)(ld-schema comps))

(defun ld-schema-p (obj)
  (and (consp obj) (eql (first obj) :schema)))
;;(ld-schema-p (ld-table-schema (first (ld-database-tables *current-database*))))

(defun ld-find-column-definition (column-designator schema-designator)
  (assert (not (null schema-designator)) t "schema-designator was nil!")
  (cond ((ld-column-p column-designator)
	 column-designator)

	((ld-identifier-p column-designator :column)
	 (find column-designator (ld-schema-column-definitions (ld-schema schema-designator))
	       :key #'ld-column-identifier
	       :test #'equal))

	((or (keywordp column-designator)
	     (ld-identifier-p column-designator))
	 (ld-find-column-definition
	  (ld-make-column-identifier column-designator schema-designator)
	  schema-designator))))

(defun ld-column-designators->coldefs (column-designators schema-designator)
  (mapcar (bind #'ld-find-column-definition schema-designator) column-designators))
;;(ld-column-designators->coldefs '(:name :age) (ld-table :users))

(provide 'ld-schema)
