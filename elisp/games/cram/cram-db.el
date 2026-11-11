;;; cram-db.el --- Cram list-database definition -*- lexical-binding: t; -*-

;;; Code:

(require 'ld-database)
(require 'ld-operations)
(require 'ld-repository)

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
      (:answer)               ; rename to :solution?
      (:picture
       :type cons)            ; cons of strings
      (:alternatives
       :type cons)            ; either a string or a cons of strings
			      ; when several answers are possible
      (:hints                 ; typically initials
       :type cons)
      ;; Glicko rating with deviation (GR RD)
      (:rating
       :type (number number))))

    (:user "Users"
     ((:id
       :type integer
       :properties (:unique :primary-key :autogenerate))
       ;; user name, which must be unique
      (:name
       :type string :properties (:unique))
      ;; Glicko rating with deviation (GR RD)
      (:rating :type (number number))))

    ;; A :match records the event of a :user encountering a :problem
    (:match "Matches"
     ((:id :type integer :properties (:unique :primary-key :autogenerate))
      ;; TODO: rename to timestamp
      (:iso-time :type string)      ; the date and time the :match was presented
      ;; TODO: rename to :response
      (:response)                   ; what the user :answered
      (:time :type integer)         ; the time (in millisecond) user spent before answering
      (:score :type number)         ; the calculated score
      (:user-id :type integer)      ; reference to match user
      (:problem-id :type integer)     ; reference to match task (should be named :problem-id) 
      ;; Glicko ratings with deviation (GR RD) just before the match
      (:user-rating :type (number number))
      (:problem-rating :type (number number)))))

  "Compact schema definitions for the Cram database.")

(defconst +cram-db-keyword+ :cram)

(defun cram-create-database ()
  "Create a new empty Cram database."
  (ld-create-database
   +cram-db-keyword+
   :schemas
   (mapcar
    (lambda (definition)
      (ld-schemadef->schema definition +cram-db-keyword+))
    +cram-db-schema-definitions+)))

(defun cram-init-database (&optional force)
  "Load or create the Cram database and make it current.
Pass FORCE to `ld-set-database'."
  (ld-set-database
   (or (ld-load-database +cram-db-keyword+)
       (cram-create-database))
   force))

(provide 'cram-db)

;;; cram-db.el ends here
