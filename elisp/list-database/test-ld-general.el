;;; test-ld-general.el --- Tests for database object resolution -*- lexical-binding: t; -*-

(require 'ert)
(require 'ld-general)

(defun ld-general-test--database ()
  "Return a database with one table and two columns."
  (ld-create-database
   :test
   :schemas
   (list
    (ld-make-schema
     :people
     :parent-keyword :test
     :column-definitions
     '((:id :type integer)
       (:name :type string))))))

(ert-deftest ld-general-detects-entity-types ()
  (let* ((database (ld-general-test--database))
         (table (car (ld-database-tables database)))
         (schema (ld-table-schema table))
         (column (car (ld-table-column-definitions table))))
    (should (eq (ld-type '(:test :people)) 'identifier))
    (should (eq (ld-type database) 'database))
    (should (eq (ld-type table) 'table))
    (should (eq (ld-type schema) 'schema))
    (should (eq (ld-type column) 'column))
    (should-error (ld-type :people) :type 'user-error)))

(ert-deftest ld-general-extracts-identifiers ()
  (let* ((database (ld-general-test--database))
         (table (car (ld-database-tables database)))
         (schema (ld-table-schema table))
         (column (car (ld-table-column-definitions table))))
    (should (equal (ld-identifier database) '(:test)))
    (should (equal (ld-identifier table) '(:test :people)))
    (should (equal (ld-identifier schema) '(:test :people)))
    (should (equal (ld-identifier column) '(:test :people :id)))
    (should (equal (ld-identifier '(:test :people))
                   '(:test :people)))))

(ert-deftest ld-general-extracts-keywords ()
  (let* ((database (ld-general-test--database))
         (table (car (ld-database-tables database))))
    (should-not (ld-keyword nil))
    (should (eq (ld-keyword :direct) :direct))
    (should (eq (ld-keyword database) :test))
    (should (eq (ld-keyword table) :people))))

(ert-deftest ld-general-extracts-database-identifiers ()
  (should (equal (ld-database-id '(:test)) '(:test)))
  (should (equal (ld-database-id '(:test :people :id)) '(:test)))
  (should-not (ld-database-id '(nil :people :id))))

(ert-deftest ld-general-resolves-objects-in-current-database ()
  (let* ((*current-database* (ld-general-test--database))
         (table (car (ld-database-tables *current-database*)))
         (column (car (ld-table-column-definitions table))))
    (should (eq (ld-object '(:test)) *current-database*))
    (should (eq (ld-object '(:test :people)) table))
    (should (eq (ld-object '(:test :people :id)) column))
    (should-not (ld-object '(:other)))
    (should-not (ld-object '(:test :missing)))
    (should-error (ld-object :people) :type 'user-error)))

(ert-deftest ld-general-resolves-database-for-model-entities ()
  (let* ((*current-database* (ld-general-test--database))
         (table (car (ld-database-tables *current-database*)))
         (schema (ld-table-schema table))
         (column (car (ld-table-column-definitions table))))
    (dolist (object (list *current-database* table schema column
                          '(:test :people :id)))
      (should (eq (ld-database object) *current-database*)))
    (should-not (ld-database '(:other :people)))))

(ert-deftest ld-general-resolves-parent-identifiers ()
  (should-not (ld-parent-id '(:test)))
  (should (equal (ld-parent-id '(:test :people)) '(:test)))
  (should (equal (ld-parent-id '(:test :people :id))
                 '(:test :people))))

(ert-deftest ld-general-resolves-parent-entities ()
  (let* ((*current-database* (ld-general-test--database))
         (table (car (ld-database-tables *current-database*)))
         (schema (ld-table-schema table))
         (column (car (ld-table-column-definitions table))))
    (should (eq (ld-parent table) *current-database*))
    (should (eq (ld-parent schema) *current-database*))
    (should (eq (ld-parent column) table))
    (should-not (ld-parent *current-database*))))

(ert-deftest ld-general-warning-prefixes-message ()
  (cl-letf (((symbol-function 'message)
             (lambda (format-string &rest arguments)
               (apply #'format format-string arguments))))
    (should (equal (ld-warning "Value %d" 3)
                   "Warning: Value 3"))))

(provide 'test-ld-general)

;;; test-ld-general.el ends here
