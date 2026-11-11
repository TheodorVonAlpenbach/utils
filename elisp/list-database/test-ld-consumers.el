;;; test-ld-consumers.el --- Tests for list-database consumers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cram-db)
(require 'maths-db)

(ert-deftest ld-consumers-create-cram-database ()
  (let* ((database (cram-create-database))
         (tables (ld-database-tables database)))
    (should (equal (ld-database-identifier database) '(:cram)))
    (should (= (length tables) 3))
    (should
     (equal (mapcar #'ld-table-identifier tables)
            '((:cram :match) (:cram :user) (:cram :problem))))
    (should
     (ld-column-property-p
      :autogenerate
      (car
       (ld-table-column-definitions
        (ld-find-table '(:cram :problem) database)))))))

(ert-deftest ld-consumers-create-maths-database ()
  (let* ((database (maths-create-database))
         (tables (ld-database-tables database)))
    (should (equal (ld-database-identifier database) '(:maths)))
    (should (= (length tables) 3))
    (should
     (equal (mapcar #'ld-table-identifier tables)
            '((:maths :matches) (:maths :users) (:maths :tasks))))))

(ert-deftest ld-consumers-cram-init-prefers-persisted-database ()
  (let ((persisted (cram-create-database))
        selected
        selected-force)
    (cl-letf (((symbol-function 'ld-load-database)
               (lambda (keyword)
                 (should (eq keyword :cram))
                 persisted))
              ((symbol-function 'ld-set-database)
               (lambda (database &optional force)
                 (setq selected database
                       selected-force force)
                 database))
              ((symbol-function 'cram-create-database)
               (lambda ()
                 (ert-fail "Fresh database should not be created"))))
      (should (eq (cram-init-database t) persisted))
      (should (eq selected persisted))
      (should selected-force))))

(ert-deftest ld-consumers-cram-init-creates-missing-database ()
  (let (selected)
    (cl-letf (((symbol-function 'ld-load-database)
               (lambda (_keyword) nil))
              ((symbol-function 'ld-set-database)
               (lambda (database &optional _force)
                 (setq selected database)
                 database)))
      (should (eq (cram-init-database) selected))
      (should (equal (ld-database-identifier selected) '(:cram))))))

(ert-deftest ld-consumers-maths-init-prefers-persisted-database ()
  (let ((persisted (maths-create-database))
        (*current-database* nil))
    (cl-letf (((symbol-function 'ld-load-database)
               (lambda (keyword)
                 (should (eq keyword :maths))
                 persisted))
              ((symbol-function 'maths-create-database)
               (lambda ()
                 (ert-fail "Fresh database should not be created"))))
      (should (eq (maths-init-database) persisted))
      (should (eq *current-database* persisted)))))

(ert-deftest ld-consumers-maths-init-creates-missing-database ()
  (let ((*current-database* nil))
    (cl-letf (((symbol-function 'ld-load-database)
               (lambda (_keyword) nil)))
      (maths-init-database)
      (should
       (equal (ld-database-identifier *current-database*)
              '(:maths))))))

(provide 'test-ld-consumers)

;;; test-ld-consumers.el ends here
