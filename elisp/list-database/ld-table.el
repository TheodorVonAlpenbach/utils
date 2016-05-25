(require 'ld-schema)

(defalias 'ld-table-schema #'second)
(defalias 'ld-table-data #'third)
(defsubst ld-table-data* (table-designator)
  (ld-table-data (ld-table table-designator)))

(defun ld-create-table (schema &optional data)
  (list :table schema data))

;; shortcuts to schema properties
(defsubst ld-table-identifier (table) (ld-schema-identifier (ld-table-schema table)))
(defsubst ld-table-name (table) (ld-schema-name (ld-table-schema table)))
(defsubst ld-table-column-definitions (table) (ld-schema-column-definitions (ld-table-schema table)))
;;(ld-schema-column-definitions '(x y z ae oe aa))

(defun ld-table-p (obj)
  (and (consp obj) (eql (first obj) :table)))
;;(ld-table-p emps)

(defun* ld-make-table-identifier (keyword &optional (database *current-database*))
  (list (ld-database-keyword database) keyword))
;;(ld-globalize-table :qwe)

(defun ld-table (table-designator)
  "Convert TABLE-DESIGNATOR to a ld-table object"
  (cond 
    ((ld-table-p table-designator) table-designator)
    ((keywordp table-designator) (ld-table (ld-make-table-identifier table-designator)))
    ((ld-identifier-p table-designator :table) (ld-find-table table-designator))
    ((ld-identifier-p table-designator :column) (ld-find-table (butlast table-designator)))
    ((ld-column-p table-designator) (ld-table (ld-identifier table-designator)))))
;;(ld-table :users)

(defun ld-schema (schema-designator)
  "Almost a corrollary to `ld-table'"
  (if (ld-schema-p schema-designator)
    schema-designator
    (ld-table-schema (ld-table schema-designator))))
;;(ld-schema :tasks)

;;; Cloning manipulations
(defun ld-clone-table (table &optional table-clone-level)
  ""
  (case table-clone-level
    ((nil) (ld-create-table (ld-table-schema table) nil))
    (:nothing nil)
    (:schema (ld-create-table (copy-tree (ld-table-schema table))))
    (:all (copy-tree table))))
;;(ld-clone-table emps)

(defun* ntable-insert-column (coldata rows &optional colpos)
  (assert (= (length coldata) (length rows)))
  (let ((trows (transpose rows)))
    (list-insert coldata (or colpos (length trows)) trows)
    (transpose trows)))
;;(let ((table '((a b) (a b) (a b)))) (list (ntable-insert-column '(1 2 3) table 2) table))

(defun* ld-table-add-column (table-designator coldef &key value colpos)
  "Returns a copy of the table designated by TABLE-DESIGNATOR,
  and with the insertion of a new column defined by COLDEF. VALUE
  is either a lisp value or a function that take a row as
  argument. COLPOS is the position of the new column after it has
  been inserted. If colpos is nil the column will be inserted as
  the last column"
  (let* ((new-table (ld-clone-table (ld-table table-designator) :all))
	 (schema (ld-schema new-table))
	 (rows (ld-table-data new-table))
	 (coldata (if (functionp value)
		    (mapcar value rows)
		    (make-list (length rows) value))))
    ;; we have to split coldef to insert the :pos keyword
    (apply #'ld-insert-column schema (first coldef) :pos colpos (rest coldef))
    (setf (ld-table-data new-table) (ntable-insert-column coldata rows colpos))
    new-table))
;;(ld-table-add-column :tasks '(:level :type integer) :colpos 2)

(provide 'ld-table)
