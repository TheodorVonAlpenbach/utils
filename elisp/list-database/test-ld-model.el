;;; test-ld-model.el --- Tests for list database model -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ld-database)

(defvar *current-database*)

(defun ld-model-test--schema (&optional database-keyword)
  "Return a representative schema under DATABASE-KEYWORD."
  (ld-make-schema
   :people
   :parent-keyword database-keyword
   :column-definitions
   '((:id :type integer :primary-key t)
     (:name :type string :unique t))))

(defun ld-model-test--table (&optional database-keyword)
  "Return a representative table under DATABASE-KEYWORD."
  (ld-create-table
   (ld-model-test--schema database-keyword)
   '((1 "Ada") (2 "Grace"))))

(ert-deftest ld-column-constructs-and-accesses-definition ()
  (let ((column
         (ld-make-column
          '(:db :people :name)
          :type 'string
          :unique t)))
    (should (ld-column-p column))
    (should (eq (ld-column-tag column) :column))
    (should (equal (ld-column-identifier column)
                   '(:db :people :name)))
    (should (equal (ld-column-name column) "name"))
    (should (eq (ld-column-type column) 'string))
    (should (equal (ld-column-properties column) '(:unique)))
    (should (eq (ld-column-keyword column) :name))))

(ert-deftest ld-column-finds-primary-key ()
  (let* ((schema (ld-model-test--schema :db))
         (column (ld-primary-key schema t)))
    (should (ld-column-primary-key-p column))
    (should (equal (ld-primary-key schema)
                   '(:db :people :id)))
    (should-not
     (ld-primary-key
      (ld-make-schema :empty :parent-keyword :db)))))

(ert-deftest ld-column-selects-comparator-by-type ()
  (should
   (eq (ld-column-comparator
        (ld-make-column '(:db :table :value) :type 'string))
       #'string=))
  (should
   (eq (ld-column-comparator
        (ld-make-column '(:db :table :value) :type 'listp))
       #'equal))
  (should
   (eq (ld-column-comparator
        (ld-make-column '(:db :table :value) :type 'integer))
       #'eql)))

(ert-deftest ld-schema-creates-columns-from-property-lists ()
  (let ((schema (ld-model-test--schema :db)))
    (should (ld-schema-p schema))
    (should (equal (ld-schema-identifier schema) '(:db :people)))
    (should (equal (ld-schema-name schema) "people"))
    (should (= (length (ld-schema-column-definitions schema)) 2))
    (should
     (equal
      (mapcar #'ld-column-keyword
              (ld-schema-column-definitions schema))
      '(:id :name)))))

(ert-deftest ld-schema-inserts-column-at-position ()
  (let ((schema (ld-model-test--schema :db)))
    (ld-insert-column schema :email :type 'string :position 1)
    (should
     (equal
      (mapcar #'ld-column-keyword
              (ld-schema-column-definitions schema))
      '(:id :email :name)))))

(ert-deftest ld-schema-rejects-invalid-column-position ()
  (let ((schema (ld-model-test--schema :db)))
    (should-error
     (ld-insert-column schema :email :position -1)
     :type 'user-error)
    (should-error
     (ld-insert-column schema :email :position 3)
     :type 'user-error)))

(ert-deftest ld-schema-converts-compact-definition ()
  (let ((schema
         (ld-schemadef->schema
          '(:tasks "Tasks" ((:id :type integer)))
          :db)))
    (should (equal (ld-schema-identifier schema) '(:db :tasks)))
    (should (equal (ld-schema-name schema) "Tasks"))
    (should (eq (ld-column-keyword
                 (car (ld-schema-column-definitions schema)))
                :id))))

(ert-deftest ld-schema-joins-column-definitions ()
  (let* ((people (ld-model-test--schema :db))
         (tasks
          (ld-make-schema
           :tasks :parent-keyword :db
           :column-definitions '((:task :type string))))
         (joined (ld-join-schemas people tasks)))
    (should
     (equal (ld-schema-identifier joined)
            '(nil :join-of-people-and-tasks)))
    (should (= (length (ld-schema-column-definitions joined)) 3))))

(ert-deftest ld-schema-finds-column-definitions ()
  (let ((schema (ld-model-test--schema :db)))
    (should
     (eq (ld-column-keyword
          (ld-find-column-definition :name schema))
         :name))
    (should-not (ld-find-column-definition :missing schema))
    (should-error
     (ld-find-column-definition :name nil)
     :type 'user-error)))

(ert-deftest ld-table-resolves-current-database-designators ()
  (let* ((table (ld-model-test--table :db))
         (*current-database*
          (ld-create-database :db :tables (list table))))
    (should (eq (ld-table :people) table))
    (should (eq (ld-table '(:db :people)) table))
    (should (eq (ld-table '(:db :people :name)) table))
    (should (eq (ld-schema :people) (ld-table-schema table)))))

(ert-deftest ld-table-clones-at-supported-levels ()
  (let ((table (ld-model-test--table :db)))
    (should-not (ld-clone-table table :nothing))
    (let ((clone (ld-clone-table table)))
      (should (eq (ld-table-schema clone) (ld-table-schema table)))
      (should-not (ld-table-data clone)))
    (let ((clone (ld-clone-table table :schema)))
      (should-not (eq (ld-table-schema clone) (ld-table-schema table)))
      (should-not (ld-table-data clone)))
    (let ((clone (ld-clone-table table :all)))
      (should (equal clone table))
      (should-not (eq clone table)))))

(ert-deftest ld-table-inserts-column-data ()
  (should
   (equal
    (ntable-insert-column '(10 20) '((1 2) (3 4)) 1)
    '((1 10 2) (3 20 4))))
  (should-error
   (ntable-insert-column '(10) '((1) (2)))
   :type 'user-error)
  (should-error
   (ntable-insert-column '(10 20) '((1) (2 3)))
   :type 'user-error))

(ert-deftest ld-table-adds-derived-column-to-copy ()
  (let* ((table (ld-model-test--table :db))
         (new-table
          (ld-table-add-column
           table
           '(:double-id :type integer)
           :value (lambda (row) (* 2 (car row)))
           :column-position 1)))
    (should (equal (ld-table-data table)
                   '((1 "Ada") (2 "Grace"))))
    (should (equal (ld-table-data new-table)
                   '((1 2 "Ada") (2 4 "Grace"))))
    (should
     (equal
      (mapcar #'ld-column-keyword
              (ld-table-column-definitions new-table))
      '(:id :double-id :name)))))

(ert-deftest ld-database-creates-and-reparents-tables ()
  (let* ((table (ld-model-test--table nil))
         (database
          (ld-create-database :directory :tables (list table))))
    (should (ld-database-p database))
    (should (eq (ld-database-keyword database) :directory))
    (should (equal (ld-database-name database) "directory"))
    (should (eq (car (ld-database-tables database)) table))
    (should
     (equal (ld-table-identifier table) '(:directory :people)))
    (should
     (cl-every
      (lambda (column)
        (eq (car (ld-column-identifier column)) :directory))
      (ld-table-column-definitions table)))))

(ert-deftest ld-database-switches-and-saves-when-forced ()
  (let* ((*current-database*
          (ld-create-database :old))
         (new (ld-create-database :new))
         saved)
    (cl-letf (((symbol-function 'ld-save-database)
               (lambda (database &optional _backup)
                 (setq saved database))))
      (should (eq (ld-set-database new t) new))
      (should (eq saved new))
      (should (eq *current-database* new)))))

(ert-deftest ld-database-decline-preserves-current-database ()
  (let* ((old (ld-create-database :old))
         (*current-database* old)
         (new (ld-create-database :new))
         saved)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil))
              ((symbol-function 'ld-save-database)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function 'message) #'ignore))
      (should-not (ld-set-database new))
      (should-not saved)
      (should (eq *current-database* old)))))

(ert-deftest ld-database-finds-and-replaces-table ()
  (let* ((table (ld-model-test--table :db))
         (database
          (ld-create-database :db :tables (list table)))
         (replacement
          (ld-create-table (ld-model-test--schema :db))))
    (should (eq (ld-find-table '(:db :people) database) table))
    (should
     (eq (ld-replace-table
          '(:db :people) replacement database)
         replacement))
    (should
     (eq (ld-find-table '(:db :people) database)
         replacement))
    (should-error
     (ld-replace-table '(:db :missing) table database)
     :type 'user-error)))

(ert-deftest ld-database-clones-tables ()
  (let* ((database
          (ld-create-database
           :db :tables (list (ld-model-test--table :db))))
         (clone (ld-clone-database database :all))
         (empty-clone (ld-clone-database database :nothing)))
    (should (equal clone database))
    (should-not (eq clone database))
    (should-not
     (eq (car (ld-database-tables clone))
         (car (ld-database-tables database))))
    (should-not (ld-database-tables empty-clone))))

(ert-deftest ld-database-renames-column ()
  (let ((table (ld-model-test--table :db)))
    (should
     (eq (ld-rename-column table :name :full-name)
         (cadr (ld-table-column-definitions table))))
    (should
     (equal
      (ld-column-identifier
       (cadr (ld-table-column-definitions table)))
      '(:db :people :full-name)))
    (should-error
     (ld-rename-column table :missing :other)
     :type 'user-error)))

(provide 'test-ld-model)

;;; test-ld-model.el ends here
