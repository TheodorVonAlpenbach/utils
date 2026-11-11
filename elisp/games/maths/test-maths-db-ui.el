;;; test-maths-db-ui.el --- Tests for Maths database access -*- lexical-binding: t; -*-

(require 'ert)
(require 'maths-db-ui)

(defun maths-db-ui-test--database ()
  "Return a populated Maths database."
  (let* ((database (maths-create-database))
         (users (ld-find-table '(:maths :users) database))
         (tasks (ld-find-table '(:maths :tasks) database)))
    (setf (ld-table-data users)
          (copy-tree
           '((2 "Bob" 9 1700 80
              (:metadata :created "2026-01-01"
               :updated "2026-01-03"))
             (1 "Ada" 8 1400 100
              (:metadata :created "2026-01-01"
               :updated "2026-01-02")))))
    (setf (ld-table-data tasks)
          (copy-tree
           '((2 :multiplication 2 (3 4) 12 1800 90
              (:metadata :created "2026-01-01"
               :updated "2026-01-03"))
             (1 :addition 1 (3 4) 7 1450 120
              (:metadata :created "2026-01-01"
               :updated "2026-01-02")))))
    database))

(ert-deftest maths-db-ui-resolves-users ()
  (let* ((*current-database* (maths-db-ui-test--database))
         (ada (maths-db-get-user "Ada")))
    (should (= (maths-user-id ada) 1))
    (should (equal (maths-db-get-user 2)
                   (car (ld-table-data* :users))))
    (should (eq (maths-db-get-user ada) ada))
    (should (= (maths-user-id* "Ada") 1))
    (should (= (maths-user-id* 7) 7))
    (should-not (maths-user-id* "missing"))))

(ert-deftest maths-db-ui-finds-latest-user-and-filters-names ()
  (let ((*current-database* (maths-db-ui-test--database)))
    (should (equal (maths-user-name (maths-db-last-user)) "Bob"))
    (should (equal (maths-db-user-names) '("Bob" "Ada")))
    (should (equal (maths-db-user-names :sans "Ada") '("Bob")))
    (should (equal (maths-db-user-names :sans '("Ada" "Bob")) nil))))

(ert-deftest maths-db-ui-resolves-tasks-and-ratings ()
  (let ((*current-database* (maths-db-ui-test--database)))
    (should (= (maths-task-id (maths-db-last-task)) 2))
    (should (= (maths-task-id (maths-db-get-task 2)) 2))
    (should (= (length (maths-db-tasks)) 2))
    (should
     (equal (maths-db-ratings-by-type :addition 1)
            '(1450)))))

(ert-deftest maths-db-ui-selects-random-task-with-empty-fallback ()
  (let* ((*current-database* (maths-db-ui-test--database))
         (tasks (ld-table :tasks)))
    (should
     (= (maths-task-id
         (maths-db-random-task
          :rating 1450 :window 1 :idle-minutes 0))
        1))
    (setf (ld-table-data tasks) nil)
    (should-not
     (maths-db-random-task
      :rating 1450 :window 100000 :idle-minutes 0))))

(ert-deftest maths-db-ui-inserts-users-and-tasks ()
  (let ((*current-database* (maths-db-ui-test--database)))
    (should
     (equal (cl-subseq
             (maths-db-insert-user "Cyd" 10 1500 100)
             0 5)
            '(3 "Cyd" 10 1500 100)))
    (should
     (equal (cl-subseq
             (maths-db-insert-task
              '(:division 2 (8 2) 4 1500 100))
             0 7)
            '(3 :division 2 (8 2) 4 1500 100)))))

(ert-deftest maths-db-ui-reports-match-and-updates-ratings ()
  (let* ((*current-database* (maths-db-ui-test--database))
         (user (maths-db-get-user 1))
         (task (maths-db-get-task 1)))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _) "now"))
              ((symbol-function 'message) #'ignore))
      (pcase-let ((`(,updated-user ,updated-task)
                   (maths-db-report-match
                    user task "2026-11-11T15:00" 7 1000
                    '(1500 90) '(1400 80))))
        (should (equal (list (maths-user-rating updated-user)
                             (maths-user-RD updated-user))
                       '(1500 90)))
        (should (equal (list (maths-task-rating updated-task)
                             (maths-task-RD updated-task))
                       '(1400 80)))
        (should (= (length (ld-table-data* :matches)) 1))))))

(provide 'test-maths-db-ui)

;;; test-maths-db-ui.el ends here
