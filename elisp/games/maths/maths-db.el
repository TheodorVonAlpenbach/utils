;;; maths-db.el --- Maths list-database definition -*- lexical-binding: t; -*-

;;; Code:

(require 'ld-database)
(require 'ld-operations)
(require 'ld-repository)

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
  "Compact schema definitions for the Maths database.")

(defconst +maths-db-keyword+ :maths)

(defun maths-create-database ()
  "Create a new empty Maths database."
  (ld-create-database
   +maths-db-keyword+
   :schemas
   (mapcar
    (lambda (definition)
      (ld-schemadef->schema definition +maths-db-keyword+))
    *maths-db-schema-definitions*)))

(defun maths-init-database ()
  "Load or create the Maths database and make it current."
  (setq *current-database*
        (or (ld-load-database +maths-db-keyword+)
            (maths-create-database))))

(provide 'maths-db)

;;; maths-db.el ends here
