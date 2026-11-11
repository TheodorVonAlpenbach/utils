;;; test-ld-identifier.el --- Tests for database identifiers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ld-identifier)

(ert-deftest ld-identifier-recognizes-keywords-and-null ()
  (should (ld-keyword-or-null-p :database))
  (should (ld-keyword-or-null-p nil))
  (should-not (ld-keyword-or-null-p 'database))
  (should-not (ld-keyword-or-null-p "database")))

(ert-deftest ld-identifier-validates-type-lengths ()
  (should (ld-identifier-p '(:database) :database))
  (should (ld-identifier-p '(:database :table) :table))
  (should (ld-identifier-p '(nil :table) :schema))
  (should (ld-identifier-p '(:database :table :column) :column))
  (should-not (ld-identifier-p '(:database :table) :database))
  (should-not (ld-identifier-p '(:database) :column)))

(ert-deftest ld-identifier-allows-only-leading-null-components ()
  (should (ld-identifier-p '(nil nil :column) :column))
  (should (ld-identifier-p '(nil :table :column) :column))
  (should-not (ld-identifier-p '(:database nil :column) :column))
  (should-not (ld-identifier-p '(nil :table nil) :column))
  (should-not (ld-identifier-p '(nil table :column) :column)))

(ert-deftest ld-identifier-selects-keyword-components ()
  (let ((identifier '(:database :table :column)))
    (should (eq (ld-identifier-keyword identifier :database) :database))
    (should (eq (ld-identifier-keyword identifier :table) :table))
    (should (eq (ld-identifier-keyword identifier :column) :column))
    (should (eq (ld-identifier-keyword identifier) :column))))

(ert-deftest ld-identifier-detects-types ()
  (should (eq (ld-identifier-type-p '(:database)) :database))
  (should (eq (ld-identifier-type-p '(nil :table)) :table))
  (should (eq (ld-identifier-type-p '(nil nil :column)) :column))
  (should-not (ld-identifier-type-p '(:database nil :column)))
  (should-not (ld-identifier-type-p nil)))

(ert-deftest ld-identifier-globalizes-column-keyword ()
  (cl-letf (((symbol-function 'ld-schema)
             (lambda (schema) schema))
            ((symbol-function 'ld-schema-identifier)
             (lambda (_schema) '(:database :table))))
    (should
     (equal (ld-make-column-identifier :column 'schema)
            '(:database :table :column)))
    (should
     (equal (ld-make-column-identifier '(:column) 'schema)
            '(:database :table :column)))))

(ert-deftest ld-identifier-qualifies-table-relative-column ()
  (cl-letf (((symbol-function 'ld-schema)
             (lambda (schema) schema))
            ((symbol-function 'ld-schema-identifier)
             (lambda (_schema) '(:database :table))))
    (should
     (equal (ld-make-column-identifier '(:table :column) 'schema)
            '(:database :table :column)))
    (should-not
     (ld-make-column-identifier '(:other :column) 'schema))))

(ert-deftest ld-identifier-preserves-global-column-identifier ()
  (let ((identifier '(:database :table :column)))
    (should
     (eq (ld-make-column-identifier identifier 'unused-schema)
         identifier))))

(ert-deftest ld-identifier-finds-column-definition ()
  (let ((columns '((:id . (:database :table :id))
                   (:name . (:database :table :name)))))
    (cl-letf (((symbol-function 'ld-schema)
               (lambda (_schema) 'schema))
              ((symbol-function 'ld-schema-identifier)
               (lambda (_schema) '(:database :table)))
              ((symbol-function 'ld-schema-column-definitions)
               (lambda (_schema) columns))
              ((symbol-function 'ld-column-identifier) #'cdr))
      (should
       (equal (ld-column-identifier-p :name 'schema)
              '(:name . (:database :table :name))))
      (should-not (ld-column-identifier-p :missing 'schema)))))

(ert-deftest ld-identifier-retains-keyword-compatibility-alias ()
  (should
   (eq (indirect-function 'keywordp-or-null)
       (indirect-function 'ld-keyword-or-null-p))))

(provide 'test-ld-identifier)

;;; test-ld-identifier.el ends here
