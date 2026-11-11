;;; ld-schema.el --- List database schemas -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'subr-x)
(require 'ld-identifier)
(require 'ld-column)

(declare-function ld-schema "ld-table")

(defalias 'ld-schema-identifier #'cadr)
(defalias 'ld-schema-name #'cl-third)
(defalias 'ld-schema-column-definitions #'cl-fourth)

(gv-define-setter ld-schema-column-definitions
    (value schema)
  `(setf (cl-fourth ,schema) ,value))

(defun ld-make-empty-schema
    (keyword name parent-keyword &optional column-definitions)
  "Create an empty schema named NAME for KEYWORD.
PARENT-KEYWORD identifies its database.  COLUMN-DEFINITIONS are
stored without conversion."
  (let ((identifier (list parent-keyword keyword)))
    (cl-assert (ld-identifier-p identifier :schema))
    (list :schema identifier name column-definitions)))

(cl-defun ld-insert-column
    (schema keyword
     &key position pos name type properties primary-key unique
     &allow-other-keys)
  "Insert a column into SCHEMA and return its definition.
KEYWORD identifies the column.  POSITION selects its zero-based
position; POS is retained as a compatibility synonym."
  (let* ((columns (ld-schema-column-definitions schema))
         (index (or position pos (length columns))))
    (unless (and (integerp index)
                 (<= 0 index)
                 (<= index (length columns)))
      (user-error "Column position is outside the schema: %S" index))
    (let ((column
           (ld-make-column
            (append (ld-schema-identifier schema) (list keyword))
            :name name :type type :properties properties
            :primary-key primary-key :unique unique)))
      (setf (ld-schema-column-definitions schema)
            (append (cl-subseq columns 0 index)
                    (list column)
                    (nthcdr index columns)))
      column)))

(defun ld-add-columns (schema columns)
  "Add column specifications in COLUMNS to SCHEMA."
  (dolist (column columns schema)
    (apply #'ld-insert-column schema column)))

(cl-defun ld-make-schema
    (keyword
     &key
     (name (substring (symbol-name keyword) 1))
     column-definitions
     (plist-p t)
     parent-keyword)
  "Create a schema identified by KEYWORD.
Process COLUMN-DEFINITIONS as property lists when PLIST-P is
non-nil; otherwise retain them as column definitions."
  (if plist-p
      (let ((schema
             (ld-make-empty-schema keyword name parent-keyword)))
        (ld-add-columns schema column-definitions))
    (ld-make-empty-schema
     keyword name parent-keyword column-definitions)))

(defun ld-schemadef->schema (schema-definition parent-keyword)
  "Convert compact SCHEMA-DEFINITION under PARENT-KEYWORD."
  (cl-destructuring-bind (keyword name columns) schema-definition
    (ld-make-schema
     keyword
     :name name
     :column-definitions columns
     :parent-keyword parent-keyword)))

(defun ld-join-schemas (&rest schemas)
  "Return a schema containing the columns of SCHEMAS."
  (when schemas
    (let ((name
           (mapconcat
            (lambda (schema)
              (downcase
               (substring
                (symbol-name
                 (ld-identifier-keyword
                  (ld-schema-identifier schema)
                  :table))
                1)))
            schemas
            "-and-")))
      (ld-make-schema
       (intern (concat ":join-of-" name))
       :column-definitions
       (apply #'append
              (mapcar #'ld-schema-column-definitions schemas))
       :plist-p nil))))

(defun ld-schema-p (object)
  "Return non-nil when OBJECT is a schema."
  (and (consp object) (eq (car object) :schema)))

(defun ld-find-column-definition
    (column-designator schema-designator)
  "Return COLUMN-DESIGNATOR's definition in SCHEMA-DESIGNATOR."
  (unless schema-designator
    (user-error "Schema designator cannot be nil"))
  (cond
   ((ld-column-p column-designator)
    column-designator)
   ((ld-identifier-p column-designator :column)
    (cl-find
     column-designator
     (ld-schema-column-definitions (ld-schema schema-designator))
     :key #'ld-column-identifier
     :test #'equal))
   ((or (keywordp column-designator)
        (ld-identifier-p column-designator))
    (when-let ((identifier
                (ld-make-column-identifier
                 column-designator schema-designator)))
      (ld-find-column-definition identifier schema-designator)))))

(defun ld-column-designators->coldefs
    (column-designators schema-designator)
  "Resolve COLUMN-DESIGNATORS in SCHEMA-DESIGNATOR."
  (mapcar
   (lambda (column)
     (ld-find-column-definition column schema-designator))
   column-designators))

(provide 'ld-schema)

;;; ld-schema.el ends here
