(require 'ld-table)

(defalias 'ld-database-identifier #'second)
(defalias 'ld-database-name #'third)
(defalias 'ld-database-tables #'fourth)

(defsubst ld-database-keyword (db)
  (first (ld-database-identifier db)))

(cl-defun ld-add-table (table db)
  "Adds TABLE to DB"
  (let ((schema (ld-table-schema table))
	(db-keyword (ld-database-keyword db)))
    (setf (first (ld-schema-identifier schema)) db-keyword)
    (cl-loop for coldef in (ld-schema-column-definitions schema)
	     do (setf (first (ld-column-identifier coldef)) db-keyword))
    (push table (ld-database-tables db))))
;;(ld-create-database :maths :schemas (mapcar #'ld-schemadef->schema *maths-db-schema-definitions*))

(cl-defun ld-set-database (ld-database &optional force)
  "Change `*current-database*' in a more safe way than with mere
setf You should always use this function when changing current
database."
  (if (null *current-database*)
    (setf *current-database* ld-database)
    (if (or force
	    (yes-or-no-p
	     (format "Current database is %s, which suggests
that an application is running, and that there are
unsaved changes. Do you want to abort now to save your
application" (ld-database-name *current-database*))))
      (progn
	(ld-save-database ld-database)
	(setf *current-database* ld-database))
      (message "Did not set database."))))

(cl-defun ld-create-database (keyword &key (name (keyword-name keyword)) schemas
				    (tables (mapcar #'ld-create-table schemas)))
  (let ((db (list :database (list keyword) name nil)))
    (dolist (table tables db) 
      (ld-add-table table db))))
(cl-indent '(ld-create-database) 'prog1)
;;(ld-create-database :maths :schemas (mapcar #'ld-make-schema *maths-db-schemas*))

(cl-defun ld-database-p (obj)
  (and (consp obj) (eql (first obj) :database)))
;;(ld-database-p *current-database*)

(cl-defun ld-clone-database (db &optional table-clone-level)
  (ld-create-database
      (ld-database-keyword db)
    :name (ld-database-name db)
    :tables (cl-loop for table in (ld-database-tables db)
		     collect (ld-clone-table table table-clone-level))))
;;(ld-clone-database deebee)

(cl-defun ld-find-table (id &optional (db *current-database*))
  "Finds table in DB by ID (NB! not keyword only)"
  (cl-find id (ld-database-tables db) :key #'ld-table-identifier :test #'equal))
;;(ld-find-table '(:maths :users))

(cl-defun ld-replace-table (id new-table &optional (db *current-database*))
  (let* ((tables (ld-database-tables db))
	(tablepos (position id tables :key #'ld-table-identifier :test #'equal)))
    (setf (nth tablepos tables) new-table)))

(cl-defun ld-rename-column (table-designator column-keyword new-keyword)
  (setf (car (cddadr
	      (cl-find column-keyword
		(ld-schema-column-definitions (ld-schema table-designator))
		:key #'ld-column-keyword)))
	new-keyword))
;;(ld-rename-column :match :answer :response)
(cddadr '(:column (:cram :problem :source-id) "SOURCE-ID" string (:unique)))


(provide 'ld-database)
