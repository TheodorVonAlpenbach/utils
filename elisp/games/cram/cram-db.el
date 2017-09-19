(require 'ld-database)
(require 'ld-operations)
(require 'ld-repository)

;;;; The DB is stored under
;;;; "/home/eier/data/ld"

(defconst +cram-db-schema-definitions+
  '((:problem "Problems"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:source-id :type string :properties (:unique)) ; unique source id
      (:text :type string)        ;one of (:addition :substraction :multiplication :division)
      (:picture :type cons)
      (:alternatives :type cons) ; cons of strings
      (:answer) ; either a string or a cons of strings when several answers are possible 
      (:rating :type cons))) ; Glicko rating with deviation (GR RD)

    (:user "Users"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:name :type string :properties (:unique))  ;user name, which must be unique
      (:rating :type cons))) ; Glicko rating with deviation (GR RD)

    ;; A :task records the event of a :user encountering a :problem
    (:task "Tasks"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:iso-time :type string)      ; the date and time the :task was presented
      (:answer)                     ; what the user :answered
      (:time :type integer)         ; the time (in millisecond) user spent before answering
      (:task-id :type integer)      ; reference to match task
      (:user-id :type integer)      ; reference to match user
      (:user-rating :type float)    ; Glicko user rating with deviation (GR RD)
      (:task-rating :type float)))) ; Glicko user rating with deviation (GR RD)

  "TODO: this is a constant, but should eventually become a variable
Note that this is a PLIST format of coldefs. Hence PLIST-P must be set to t in `ld-make-schema'")

;;(first (fourth (second (first (fourth *cram-db*)))))
;;(ld-table-column-definitions (ld-table :users))

(defun cram-update-schema (keyword)
  (setf (ld-table-schema (ld-find-table (list +cram-db-keyword+ keyword) *cram-db*))
	(ld-schemadef->schema (find keyword +cram-db-schema-definitions+
				    :key #'first))))
;;(cram-update-schema :users)

(defconst +cram-db-keyword+ :cram)

;;(require 'ld-repository)
(defun cram-init-database ()
  "TODO: this is a constant, but should eventually become a variable"
  (setf *current-database*
	(or (ld-load-database +cram-db-keyword+)
	    (ld-create-database +cram-db-keyword+ 
				:schemas (mapcar (bind #'ld-schemadef->schema +cram-db-keyword+)
						 +cram-db-schema-definitions+)))))
;;(cram-init-database)
;;(setf *dbcopy* (copy-tree *current-database*))
;;(setf *current-database* (copy-tree *dbcopy*))
;;(ld-replace-table '(:cram :tasks) (ld-table-add-column :tasks '(:level :type integer) :colpos 2))
;;(ld-table :tasks)
;;(loop for task in (cram-db-tasks) do (setf (cram-task-level task) (cram-estimate-level task)))

(provide 'cram-db)
