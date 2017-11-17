(require 'ert)

(ert-deftest test-init-db ()
  "Test of `combine'"
  (let ((db (cram-init-database t)))
    (should db)
    (let ((name "Ludvik"))
      (should (cram-add-user name))
      (should (cram-db-get-user name))
      (should-not (cram-db-get-user "Marduk"))
      (should (cram-user-id* name))
      (should (equal (cram-user-id* -1) -1))
      (should-not (cram-user-id* -1 t))
      (should (equal (cram-db-user-names) (list name)))
      (should-not (cram-db-user-names :sans name)))))



