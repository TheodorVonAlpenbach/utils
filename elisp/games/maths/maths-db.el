(require 'ld-database)
(require 'ld-operations)
(require 'ld-repository)

;;;; The DB is stored under
;;;; "/home/eier/data/ld"

(defconst *maths-db-schema-definitions*
  '((:tasks "Tasks"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:operation :type symbol)        ;one of (:addition :substraction :multiplication :division)
      (:level :type integer)
      (:arguments :type cons)
      (:solution)
      (:rating :type float)     ;current Glicko rating
      (:RD :type float)))       ;current Glicko rating deviation

    (:users "Users"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:name :type string :properties (:unique))  ;user name, which must be unique
      (:age :type integer)
      (:rating :type float)     ;current Glicko rating
      (:RD :type float)))       ;current Glicko rating deviation

    (:matches "Matches"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:iso-time :type string)         ;the date and time the task was presented
      (:answer)
      (:time :type integer)             ;the time (in millisecond) user spent before answering
      (:task-id :type integer)         ;reference to match task
      (:user-id :type integer)         ;reference to match user
      (:user-rating :type float)      ;user rating right before match
      (:user-RD :type float)          ;d.o. user RD
      (:task-rating :type float)      ;task rating right before match
      (:task-RD :type float))))       ;d.o. task RD
  "TODO: this is a constant, but should eventually become a variable
Note that this is a PLIST format of coldefs. Hence PLIST-P must be set to t in `ld-make-schema'")

;;(first (fourth (second (first (fourth *maths-db*)))))
;;(ld-table-column-definitions (ld-table :users))

(defun maths-update-schema (keyword)
  (setf (ld-table-schema (ld-find-table (list +maths-db-keyword+ keyword) *maths-db*))
	(ld-schemadef->schema (find keyword *maths-db-schema-definitions*
				    :key #'first))))
;;(maths-update-schema :users)

(defconst +maths-db-keyword+ :maths)

;;(require 'ld-repository)
(defun maths-init-database ()
  "TODO: this is a constant, but should eventually become a variable"
  (setf *current-database*
	(or (ld-load-database +maths-db-keyword+)
	    (ld-create-database +maths-db-keyword+ 
				:schemas (mapcar (bind #'ld-schemadef->schema +maths-db-keyword+)
						 *maths-db-schema-definitions*)))))
;;(maths-init-database)
;;(setf *dbcopy* (copy-tree *current-database*))
;;(setf *current-database* (copy-tree *dbcopy*))
;;(ld-replace-table '(:maths :tasks) (ld-table-add-column :tasks '(:level :type integer) :colpos 2))
;;(ld-table :tasks)
;;(loop for task in (maths-db-tasks) do (setf (maths-task-level task) (maths-estimate-level task)))

(provide 'maths-db)
