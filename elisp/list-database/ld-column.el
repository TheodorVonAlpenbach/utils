;; column definitions == (:column keyword name type)
(cl-defun ld-make-column (id &key name type properties)
  "If TYPE is nil: no particular type. Otherwise use the type symbols in elisp."
  (list :column id (or name (keyword-name (ld-identifier-keyword id :column))) type properties))
;;(ld-make-column :qwe)

(defalias 'ld-column-tag #'first)
(defalias 'ld-column-identifier #'second)
(defalias 'ld-column-name #'third)
(defalias 'ld-column-type #'fourth)
(defalias 'ld-column-properties #'fifth)

(cl-defun ld-column-property-p (property coldef)
  (member property (ld-column-properties coldef)))
(cl-defun ld-column-primary-key-p (coldef)
  (ld-column-property-p :primary-key coldef))

(cl-defun ld-column-p (obj)
  (and (consp obj) (eql (first obj) :column)))

(cl-defun ld-column-keyword (coldef)
  (ld-identifier-keyword (ld-column-identifier coldef)))

(cl-defun ld-primary-key (schema &optional as-column-definition-p)
  "Returns the column that is the primary key. If
AS-COLUMN-DEFINITION-P is non nil the schema definition for that
column is returned."
  (let ((coldef (find-if #'ld-column-primary-key-p
		  (ld-schema-column-definitions schema))))
    (if as-column-definition-p
      coldef (ld-column-identifier coldef))))
;;(ld-primary-key (ld-schema :users))

(cl-defun ld-column-comparator (coldef)
  (cl-case (ld-column-type coldef)
    (string #'string=)
    (listp #'equal)
    (otherwise #'eql)))

(provide 'ld-column)
