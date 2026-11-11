;;; test-list-db.el --- Tests for the list-database entry point -*- lexical-binding: t; -*-

(require 'ert)
(require 'list-db)

(ert-deftest list-db-loads-complete-public-surface ()
  (should (featurep 'ld-database))
  (should (featurep 'ld-operations))
  (should (featurep 'ld-repository))
  (should (fboundp 'ld-create-database))
  (should (fboundp 'ld-select-1))
  (should (fboundp 'ld-save-database))
  (should (fboundp 'ld-load-database)))

(ert-deftest list-db-entry-supports-basic-database-flow ()
  (let* ((schema
          (ld-schemadef->schema
           '(:people "People"
             ((:id
               :type integer
               :properties
               (:unique :primary-key :autogenerate))
              (:name
               :type string
               :properties (:unique))))
           :example))
         (*current-database*
          (ld-create-database
           :example :schemas (list schema))))
    (ld-insert :people '("Ada"))
    (should
     (equal
      (ld-select :people :column :name)
      '("Ada")))))

(provide 'test-list-db)

;;; test-list-db.el ends here
