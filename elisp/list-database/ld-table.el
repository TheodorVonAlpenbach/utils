;;; ld-table.el --- List database tables -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'subr-x)
(require 'ld-schema)

(defvar *current-database*)

(declare-function ld-database-keyword "ld-database")
(declare-function ld-find-table "ld-database")

(defalias 'ld-table-schema #'cadr)
(defalias 'ld-table-data #'cl-third)

(gv-define-setter ld-table-data (value table)
  `(setf (cl-third ,table) ,value))

(defun ld-table-data* (table-designator)
  "Return the data in TABLE-DESIGNATOR."
  (ld-table-data (ld-table table-designator)))

(defun ld-create-table (schema &optional data)
  "Create a table from SCHEMA and DATA."
  (list :table schema data))

(defun ld-table-identifier (table)
  "Return TABLE's identifier."
  (ld-schema-identifier (ld-table-schema table)))

(defun ld-table-name (table)
  "Return TABLE's name."
  (ld-schema-name (ld-table-schema table)))

(defun ld-table-column-definitions (table)
  "Return TABLE's column definitions."
  (ld-schema-column-definitions (ld-table-schema table)))

(defun ld-table-p (object)
  "Return non-nil when OBJECT is a table."
  (and (consp object) (eq (car object) :table)))

(defun ld-make-table-identifier
    (keyword &optional database)
  "Return KEYWORD's table identifier in DATABASE.
DATABASE defaults to `*current-database*'."
  (list (ld-database-keyword
         (or database *current-database*))
        keyword))

(defun ld-table (table-designator)
  "Resolve TABLE-DESIGNATOR to a table, or return nil."
  (cond
   ((ld-table-p table-designator)
    table-designator)
   ((keywordp table-designator)
    (ld-table (ld-make-table-identifier table-designator)))
   ((ld-identifier-p table-designator :table)
    (ld-find-table table-designator))
   ((ld-identifier-p table-designator :column)
    (ld-find-table (butlast table-designator)))
   ((ld-column-p table-designator)
    (ld-table
     (butlast (ld-column-identifier table-designator))))))

(defun ld-schema (schema-designator)
  "Resolve SCHEMA-DESIGNATOR to a schema, or return nil."
  (if (ld-schema-p schema-designator)
      schema-designator
    (when-let ((table (ld-table schema-designator)))
      (ld-table-schema table))))

(defun ld-clone-table (table &optional table-clone-level)
  "Clone TABLE according to TABLE-CLONE-LEVEL.
Nil retains only the original schema.  `:nothing' returns nil,
`:schema' copies the schema, and `:all' copies the whole table."
  (pcase table-clone-level
    ('nil (ld-create-table (ld-table-schema table)))
    (:nothing nil)
    (:schema (ld-create-table (copy-tree (ld-table-schema table))))
    (:all (copy-tree table))
    (_ (user-error "Unknown table clone level: %S"
                   table-clone-level))))

(defun ntable-insert-column (column-data rows &optional column-position)
  "Return ROWS with COLUMN-DATA inserted at COLUMN-POSITION."
  (unless (= (length column-data) (length rows))
    (user-error "Column data and row counts differ"))
  (let* ((width (if rows (length (car rows)) 0))
         (position (or column-position width)))
    (unless (and (integerp position)
                 (<= 0 position)
                 (<= position width)
                 (cl-every
                  (lambda (row) (= (length row) width))
                  rows))
      (user-error "Invalid table rows or column position"))
    (cl-mapcar
     (lambda (row value)
       (append (cl-subseq row 0 position)
               (list value)
               (nthcdr position row)))
     rows column-data)))

(cl-defun ld-table-add-column
    (table-designator column-definition
     &key value column-position colpos)
  "Return a copy of TABLE-DESIGNATOR with a new column.
COLUMN-DEFINITION is a column property-list specification.  VALUE
is either a constant or a function of each row.  COLUMN-POSITION
selects the insertion point; COLPOS is a compatibility synonym."
  (let* ((position (or column-position colpos))
         (new-table
          (ld-clone-table (ld-table table-designator) :all))
         (schema (ld-schema new-table))
         (rows (ld-table-data new-table))
         (column-data
          (if (functionp value)
              (mapcar value rows)
            (make-list (length rows) value))))
    (apply #'ld-insert-column
           schema
           (car column-definition)
           :position position
           (cdr column-definition))
    (setf (ld-table-data new-table)
          (ntable-insert-column column-data rows position))
    new-table))

(provide 'ld-table)

;;; ld-table.el ends here
