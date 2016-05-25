;;;; Identifiers are on the form (database-keyword &optional table-keyword column-keyword)
;;;; For databases the following forms are allowed: (nil) (database-keyword)
;;;; Tables (database-keyword table-keyword) (nil table-keyword)
;;;; Columns (database-keyword table-keyword column-keyword) (nil table-keyword column-keyword) (nil nil column-keyword)

(defun keywordp-or-null (x)
  (or (keywordp x) (null x)))

(defun ld-identifier-p (form &optional type)
  "If this function returns true, FORM is interpreted as a an identifier.
Identifers are used for databases, tables, schemas columns"
  (and (consp form)
       (case type
	 (:database (= (length form) 1))
	 ((:table :schema) (= (length form) 2))
	 (:column (= (length form) 3))
	 (otherwise t))
       (every #'keywordp-or-null form)
       (awhen (member-if #'keywordp form)
	 (every #'keywordp it))))
;;(mapcar #'ld-identifier-p (list '(nil nil nil) '(nil nil :qwe) '(nil :ewq nil) '(nil :ewq :qwe) '(:top nil nil) '(:top nil :qwe) '(:top :ewq nil) '(:top :ewq :qwe)))
;;(mapcar #'ld-identifier-p (list '(nil nil) '(nil :ewq) '(:top nil) '(:top :ewq)))
;;(mapcar #'ld-identifier-p (list '(nil) '(:top) nil))
;;(mapcar (bind #'ld-identifier-p '(:top :ewq :qwe) 1) '(:database :table :column))
;;(ld-identifier-p '(round :rating))

(defun ld-identifier-keyword (id &optional type)
  (if type
    (case type
      (:database (first id))
      (:table (second id))
      (:column (third id)))
    (last-elt id)))
;;(mapcar (bind #'ld-identifier-keyword '(:a :b :c) 1) '(:database :table :column))
;;(mapcar #'ld-identifier-keyword '((:a :b :c) (:a :b) (:a)))

(defun ld-column-identifier-p (colid-designator schema-designator)
  (let* ((schema (ld-schema schema-designator))
	 (colid (ld-make-column-identifier colid-designator schema)))
    (find colid (ld-schema-column-definitions schema) :key #'ld-column-identifier :test #'equal)))
;;(mapcar (compose #'null #'null (bind #'ld-column-identifier-p :users)) (list '(:name) :name '(:users :name) '(:maths :users :name) '(:nomaths :users :name)))

(defun* ld-make-column-identifier (colid-designator schema-designator)
  "COLID-DESIGNATOR is either a keyword or a list one, two or three keywords.
In the case of a two keywords list, the butlast elements must
match the identifer of schema-designator.

Or maybe we should rather then assume that it is indeed antoher
schema, and use the database from schema-designator to globalize
colid?"
  (if (ld-identifier-p colid-designator :column)
    colid-designator
    (let ((tabid (ld-schema-identifier (ld-schema schema-designator))))
      (cond ((keywordp colid-designator) (append tabid (list colid-designator)))

	    ((ld-identifier-p colid-designator :table) ;hack: just detecting length 2
	     (when (eql (second tabid) (first colid-designator))
	       (cons (first tabid) colid-designator)))

	    ((ld-identifier-p colid-designator :database)
	     (append tabid colid-designator))))))
;;(mapcar (bind #'ld-make-column-identifier :users) (list '(:qwe) :qwe '(:users :qwe) '(:maths :users :qwe) '(:nomaths :users :qwe)))

(defun ld-identifier-type-p (id)
  (when (ld-identifier-p id)
    (case (length id) (1 :database) (2 :table) (3 :column))))
;;(mapcar #'ld-identifier-type-p '((nil) (nil :ewq) (:foo :ewq :qwe)))

(provide 'ld-identifier)
