;;; test-ld-operations.el --- Tests for list-database operations -*- lexical-binding: t; -*-

(require 'ert)
(require 'ld-operations)

(defun ld-operations-test--table ()
  "Return a fresh table used by operation tests."
  (ld-create-table
   (ld-make-schema
    :people
    :parent-keyword :test
    :column-definitions
    '((:id :type integer :properties (:primary-key :unique :autogenerate))
      (:name :type string :unique t)
      (:score :type integer)))))

(ert-deftest ld-operations-validates-rows ()
  (let* ((table (ld-operations-test--table))
         (schema (ld-table-schema table)))
    (should (ld-check-row '(1 "Ada" 10) schema))
    (should-not (ld-check-row '(1 "Ada") schema))
    (should-not (ld-check-row '(1 2 10) schema))
    (should (ld-check-rows '((1 "Ada" 10) (2 "Bob" 20)) schema))))

(ert-deftest ld-operations-inserts-raw-rows ()
  (let ((table (ld-operations-test--table)))
    (should (equal (ld-insert-row table '(1 "Ada" 10) '(2 "Bob" 20))
                   '((1 "Ada" 10) (2 "Bob" 20))))
    (should-error
     (ld-insert-row table '(3 "incomplete"))
     :type 'user-error)))

(ert-deftest ld-operations-resolves-column-positions-and-values ()
  (let* ((table (ld-operations-test--table))
         (columns (ld-table-column-definitions table))
         (name (cadr columns)))
    (should (= (ld-column-position name table) 1))
    (should (= (ld-column-position '(:test :people :score) table) 2))
    (should (equal (ld-row-value '(1 "Ada" 10) name table) "Ada"))
    (should-not
     (ld-column-position '(:test :people :missing) table))))

(ert-deftest ld-operations-autogenerates-values ()
  (let* ((table (ld-operations-test--table))
         (id (car (ld-table-column-definitions table))))
    (should (= (ld-autogenerate id table) 1))
    (setf (ld-table-data table) '((7 "Ada" 10)))
    (should (= (ld-autogenerate id table) 8))))

(ert-deftest ld-operations-compiles-complete-and-partial-rows ()
  (let ((table (ld-operations-test--table)))
    (should (equal (ld-compile-new-row table '("Ada" 10) nil)
                   '(1 "Ada" 10)))
    (should
     (equal
      (ld-compile-new-row table '(20 "Bob") '(:score :name))
      '(1 "Bob" 20)))
    (should-error
     (ld-compile-new-row table '(3 "Ada" 10) '(:id :name :score))
     :type 'user-error)
    (should-error
     (ld-compile-new-row table '("Ada") nil)
     :type 'user-error)))

(ert-deftest ld-operations-rejects-duplicate-unique-values ()
  (let ((table (ld-operations-test--table)))
    (setf (ld-table-data table) '((1 "Ada" 10)))
    (should (ld-value-exists-p "Ada" table
                               (cadr (ld-table-column-definitions table))))
    (should-error
     (ld-compile-new-row table '("Ada" 20) nil)
     :type 'user-error)))

(ert-deftest ld-operations-creates-row-metadata ()
  (should
   (equal (ld-create-metadata "2026-11-11T14:00:00+0100")
          '(:metadata
            :created "2026-11-11T14:00:00+0100"
            :updated "2026-11-11T14:00:00+0100"))))

(ert-deftest ld-operations-reads-and-updates-row-metadata ()
  (let* ((metadata
          '(:metadata :created "then" :updated "then"))
         (row (list 1 "Ada" 10 metadata)))
    (should (eq (ld-row-metadata row) metadata))
    (should (equal (ld-get-metadatum :created metadata) "then"))
    (should (equal (ld-set-metadatum "now" :updated metadata) "now"))
    (should (equal (ld-get-metadatum :updated metadata) "now"))))

(ert-deftest ld-operations-inserts-row-with-metadata ()
  (let ((table (ld-operations-test--table)))
    (cl-letf (((symbol-function 'ld-create-metadata)
               (lambda (&optional _)
                 '(:metadata :created "now" :updated "now"))))
      (should
       (equal (ld-insert table '("Ada" 10))
              '(1 "Ada" 10
                (:metadata :created "now" :updated "now"))))
      (should (= (length (ld-table-data table)) 1)))))

(ert-deftest ld-operations-selects-filters-sorts-and-projects ()
  (let ((table (ld-operations-test--table)))
    (setf (ld-table-data table)
          '((1 "Ada" 10) (2 "Bob" 30) (3 "Cyd" 20)))
    (should
     (equal
      (ld-select-1
       table
       :where (lambda (row) (>= (nth 2 row) 20))
       :order :descending
       :order-by 2
       :columns
       (list (lambda (row) (nth 1 row))
             (lambda (row) (nth 2 row))))
      '(("Bob" 30) ("Cyd" 20))))
    (should
     (equal
      (ld-select-1 table :column (lambda (row) (nth 1 row)))
      '("Ada" "Bob" "Cyd")))))

(ert-deftest ld-operations-formats-selected-rows ()
  (let ((table (ld-operations-test--table)))
    (setf (ld-table-data table) '((1 "Ada" 10) (2 "Bob" 20)))
    (should
     (equal
      (ld-select-1
       table
       :columns
       (list (lambda (row) (nth 1 row))
             (lambda (row) (nth 2 row)))
       :format "%s=%d\n")
      "Ada=10\nBob=20\n"))))

(ert-deftest ld-operations-deletes-and-updates-selected-rows ()
  (let ((table (ld-operations-test--table)))
    (setf (ld-table-data table)
          '((1 "Ada" 10
             (:metadata :created "then" :updated "then"))
            (2 "Bob" 20
             (:metadata :created "then" :updated "then"))))
    (ld-delete-if (lambda (row) (= (car row) 2)) table)
    (should (= (length (ld-table-data table)) 1))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _) "now")))
      (should
       (equal (ld-update-1 table
                           (lambda (row) (= (car row) 1))
                           15
                           2)
              '(1 "Ada" 15
                (:metadata :created "then" :updated "now")))))))

(provide 'test-ld-operations)

;;; test-ld-operations.el ends here
