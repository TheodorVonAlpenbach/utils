(require 'ld-table)

(defalias 'ld-database-identifier #'second)
(defalias 'ld-database-name #'third)
(defalias 'ld-database-tables #'fourth)

(defsubst ld-database-keyword (db)
  (first (ld-database-identifier db)))

(defun ld-add-table (table db)
  "Adds TABLE to DB"
  (let ((schema (ld-table-schema table))
	(db-keyword (ld-database-keyword db)))
    (setf (first (ld-schema-identifier schema)) db-keyword)
    (loop for coldef in (ld-schema-column-definitions schema)
	  do (setf (first (ld-column-identifier coldef)) db-keyword))
    (push table (ld-database-tables db))))
;;(ld-create-database :maths :schemas (mapcar #'ld-schemadef->schema *maths-db-schema-definitions*))

(cl-defun ld-create-database (keyword &key (name (keyword-name keyword)) schemas
				    (tables (mapcar #'ld-create-table schemas)))
  (let ((db (list :database (list keyword) name nil)))
    (dolist (table tables db) 
      (ld-add-table table db))))
;;(ld-create-database :maths :schemas (mapcar #'ld-make-schema *maths-db-schemas*))

(defun ld-database-p (obj)
  (and (consp obj) (eql (first obj) :database)))
;;(ld-database-p *current-database*)

(cl-defun ld-clone-database (db &optional table-clone-level)
  (ld-create-database
   (ld-database-keyword db)
   :name (ld-database-name db)
   :tables (loop for table in (ld-database-tables db)
		 collect (ld-clone-table table table-clone-level))))
;;(ld-clone-database deebee)

(cl-defun ld-find-table (id &optional (db *current-database*))
  "Finds table in DB by ID (NB! not keyword only)"
  (find id (ld-database-tables db) :key #'ld-table-identifier :test #'equal))
;;(ld-find-table '(:maths :users))

(cl-defun ld-replace-table (id new-table &optional (db *current-database*))
  (let* ((tables (ld-database-tables db))
	(tablepos (position id tables :key #'ld-table-identifier :test #'equal)))
    (setf (nth tablepos tables) new-table)))

(provide 'ld-database)
