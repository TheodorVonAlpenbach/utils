;;; test-ld-repository.el --- Tests for list-database persistence -*- lexical-binding: t; -*-

(require 'ert)
(require 'ld-repository)

(defun ld-repository-test--database (&optional empty)
  "Return a test database, with no tables when EMPTY is non-nil."
  (if empty
      (ld-create-database :test)
    (let ((database
           (ld-create-database
            :test
            :schemas
            (list
             (ld-make-schema
              :people
              :parent-keyword :test
              :column-definitions
              '((:id :type integer)
                (:name :type string)))))))
      (setf (ld-table-data
             (car (ld-database-tables database)))
            '((1 "Ada") (2 "Bob")))
      database)))

(ert-deftest ld-repository-creates-directory-names ()
  (should (equal (ld-database-directory-name :test) "test"))
  (should (equal (ld-database-directory-name nil) "free-tables"))
  (should (equal (ld-database-directory-name :test "-copy")
                 ".test-copy"))
  (should-error
   (ld-database-directory-name :test :unsupported)
   :type 'user-error))

(ert-deftest ld-repository-creates-dated-backup-name ()
  (cl-letf (((symbol-function 'format-time-string)
             (lambda (&rest _) "20261111T1447")))
    (should
     (equal (ld-database-directory-name :test :iso-date)
            ".test-20261111T1447"))))

(ert-deftest ld-repository-builds-database-paths ()
  (let ((ld-repository-directory "/tmp/list-databases"))
    (should
     (equal (ld-database-repository :test)
            "/tmp/list-databases/test"))
    (should
     (equal (ld-database-file :test)
            "/tmp/list-databases/test/metadata.eld"))))

(ert-deftest ld-repository-builds-table-paths ()
  (let* ((ld-repository-directory "/tmp/list-databases")
         (*current-database* (ld-repository-test--database))
         (table (car (ld-database-tables *current-database*))))
    (should (equal (ld-table-filename table) "people.eld"))
    (should
     (equal (ld-table-file table)
            "/tmp/list-databases/test/people.eld"))
    (should
     (equal (ld-table-file :people)
            "/tmp/list-databases/test/people.eld"))))

(ert-deftest ld-repository-prints-without-abbreviation ()
  (should
   (equal
    (with-output-to-string
      (print-all '((1 2 3) (4 5 6)) standard-output #'prin1))
    "((1 2 3) (4 5 6))")))

(ert-deftest ld-repository-saves-and-loads-table ()
  (let* ((directory (make-temp-file "ld-repository-" t))
         (ld-repository-directory directory)
         (*current-database* (ld-repository-test--database))
         (table (car (ld-database-tables *current-database*))))
    (unwind-protect
        (let ((path (ld-save-table table)))
          (should (file-readable-p path))
          (should
           (equal (ld-load-table (ld-table-identifier table))
                  table)))
      (delete-directory directory t))))

(ert-deftest ld-repository-reports-missing-table ()
  (let* ((directory (make-temp-file "ld-repository-" t))
         (ld-repository-directory directory))
    (unwind-protect
        (should-error
         (ld-load-table '(:test :missing))
         :type 'user-error)
      (delete-directory directory t))))

(ert-deftest ld-repository-round-trips-database ()
  (let* ((directory (make-temp-file "ld-repository-" t))
         (ld-repository-directory directory)
         (database (ld-repository-test--database)))
    (unwind-protect
        (progn
          (ld-save-database database)
          (should (equal (ld-load-database :test) database))
          (should-not (ld-load-database :missing)))
      (delete-directory directory t))))

(ert-deftest ld-repository-persists-empty-database ()
  (let* ((directory (make-temp-file "ld-repository-" t))
         (ld-repository-directory directory)
         (database (ld-repository-test--database t)))
    (unwind-protect
        (let ((path (ld-save-database database)))
          (should (file-readable-p path))
          (should (equal (ld-load-database :test) database)))
      (delete-directory directory t))))

(provide 'test-ld-repository)

;;; test-ld-repository.el ends here
