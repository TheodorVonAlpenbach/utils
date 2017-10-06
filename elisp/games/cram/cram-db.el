(require 'ld-database)
(require 'ld-operations)
(require 'ld-repository)

;;;; The DB is stored under
;;;; "/home/eier/data/ld"

(defconst +cram-db-schema-definitions+
  '((:problem "Problems"
     ((:id
       :type integer
       :properties (:unique :primary-key :autogenerate))
      (:source-id
       :type string
       :properties (:unique)) ; unique source id
      (:question
       :type string)
      (:answer)
      (:picture
       :type cons)            ; cons of strings
      (:alternatives
       :type cons)            ; either a string or a cons of strings
			      ; when several answers are possible
      (:hints                 ; typically initials
       :type cons)
      (:rating
       :type cons)))          ; Glicko rating with deviation (GR RD)

    (:user "Users"
     ((:id
       :type integer
       :properties (:unique :primary-key :autogenerate))
      (:name
       :type string
       :properties (:unique))  ;user name, which must be unique
      (:rating
       :type cons))) ; Glicko rating with deviation (GR RD)

    ;; A :match records the event of a :user encountering a :problem
    (:match "Matches"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      (:iso-time :type string)      ; the date and time the :match was presented
      (:answer)                     ; what the user :answered
      (:time :type integer)         ; the time (in millisecond) user spent before answering
      (:match-id :type integer)      ; reference to match task
      (:user-id :type integer)      ; reference to match user
      (:user-rating :type float)    ; Glicko user rating with deviation (GR RD)
      (:match-rating :type float)))) ; Glicko user rating with deviation (GR RD)

  "TODO: this is a constant, but should eventually become a variable
Note that this is a PLIST format of coldefs. Hence PLIST-P must be set to t in `ld-make-schema'")

;;(first (fourth (second (first (fourth *cram-db*)))))
;;(ld-table-column-definitions (ld-table :users))

(defun cram-update-schema (keyword)
  (setf (ld-table-schema
	 (ld-find-table (list +cram-db-keyword+ keyword) *cram-db*))
	(ld-schemadef->schema (find keyword +cram-db-schema-definitions+
				    :key #'first))))
;;(cram-update-schema :user)

(defconst +cram-db-keyword+ :cram)

;;(require 'ld-repository)
(defun cram-init-database (&optional force)
  "TODO: this is a constant, but should eventually become a variable"
  (ld-set-database
   (or (ld-load-database +cram-db-keyword+)
       (ld-create-database +cram-db-keyword+ 
	 :schemas (mapcar (bind #'ld-schemadef->schema +cram-db-keyword+)
		    +cram-db-schema-definitions+)))
   force))
;;(cram-init-database)
;;(setf *current-database* nil)
;;(setf *dbcopy* (copy-tree *current-database*))
;;(setf *current-database* (copy-tree *dbcopy*))
;;(ld-replace-table '(:cram :matchs) (ld-table-add-column :matchs '(:level :type integer) :colpos 2))
;;(ld-table :matchs)
;;(loop for task in (cram-db-tasks) do (setf (cram-task-level task) (cram-estimate-level task)))

(provide 'cram-db)
