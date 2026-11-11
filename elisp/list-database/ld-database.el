;;; ld-database.el --- List database containers -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'ld-table)

(defvar *current-database*)

(declare-function ld-save-database "ld-repository")

(defalias 'ld-database-identifier #'cadr)
(defalias 'ld-database-name #'cl-third)
(defalias 'ld-database-tables #'cl-fourth)

(gv-define-setter ld-database-tables
    (value database)
  `(setf (cl-fourth ,database) ,value))

(defun ld-database-keyword (database)
  "Return DATABASE's keyword."
  (car (ld-database-identifier database)))

(defun ld-add-table (table database)
  "Add TABLE to DATABASE and return TABLE.
Rewrite the database component of its schema and column
identifiers to match DATABASE."
  (let ((schema (ld-table-schema table))
        (database-keyword (ld-database-keyword database)))
    (setf (car (ld-schema-identifier schema)) database-keyword)
    (dolist (column (ld-schema-column-definitions schema))
      (setf (car (ld-column-identifier column)) database-keyword))
    (push table (ld-database-tables database))
    table))

(defun ld-set-database (database &optional force)
  "Make DATABASE the value of `*current-database*'.
When another database is active, save DATABASE before switching
when FORCE is non-nil or the user confirms the operation.  Return
DATABASE after switching and nil when the user declines."
  (if (null *current-database*)
      (setq *current-database* database)
    (if (or force
            (yes-or-no-p
             (format
              "Save %s and replace current database %s? "
              (ld-database-name database)
              (ld-database-name *current-database*))))
        (progn
          (ld-save-database database)
          (setq *current-database* database))
      (message "Did not set database")
      nil)))

(cl-defun ld-create-database
    (keyword
     &key
     (name (substring (symbol-name keyword) 1))
     schemas
     (tables (mapcar #'ld-create-table schemas)))
  "Create a database identified by KEYWORD.
NAME defaults to the keyword name.  SCHEMAS are converted to
empty tables unless TABLES is supplied."
  (let ((database (list :database (list keyword) name nil)))
    (dolist (table tables database)
      (ld-add-table table database))))

(put 'ld-create-database 'lisp-indent-function 1)
(put 'ld-create-database 'common-lisp-indent-function 1)

(defun ld-database-p (object)
  "Return non-nil when OBJECT is a database."
  (and (consp object) (eq (car object) :database)))

(defun ld-clone-database
    (database &optional table-clone-level)
  "Clone DATABASE's tables according to TABLE-CLONE-LEVEL."
  (ld-create-database
   (ld-database-keyword database)
   :name (ld-database-name database)
   :tables
   (delq
    nil
    (mapcar
     (lambda (table)
       (ld-clone-table table table-clone-level))
     (ld-database-tables database)))))

(defun ld-find-table (identifier &optional database)
  "Find table IDENTIFIER in DATABASE.
DATABASE defaults to `*current-database*'."
  (cl-find identifier
           (ld-database-tables
            (or database *current-database*))
           :key #'ld-table-identifier
           :test #'equal))

(defun ld-replace-table
    (identifier new-table &optional database)
  "Replace IDENTIFIER with NEW-TABLE in DATABASE.
DATABASE defaults to `*current-database*'.  Report a `user-error'
when IDENTIFIER is absent."
  (let* ((target (or database *current-database*))
         (tables (ld-database-tables target))
         (position
          (cl-position identifier tables
                       :key #'ld-table-identifier
                       :test #'equal)))
    (unless position
      (user-error "Database has no table %S" identifier))
    (setf (nth position tables) new-table)
    new-table))

(defun ld-rename-column
    (table-designator column-keyword new-keyword)
  "Rename COLUMN-KEYWORD in TABLE-DESIGNATOR to NEW-KEYWORD."
  (let ((column
         (cl-find column-keyword
                  (ld-schema-column-definitions
                   (ld-schema table-designator))
                  :key #'ld-column-keyword)))
    (unless column
      (user-error "Table has no column %S" column-keyword))
    (setf (car (last (ld-column-identifier column))) new-keyword)
    column))

(provide 'ld-database)

;;; ld-database.el ends here
