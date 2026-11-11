;;; test-cram-db-ui.el --- Tests for Cram database access -*- lexical-binding: t; -*-

(require 'ert)
(require 'cram-db-ui)

(defun cram-db-ui-test--database ()
  "Return a populated Cram database."
  (let* ((database (cram-create-database))
         (users (ld-find-table '(:cram :user) database))
         (problems (ld-find-table '(:cram :problem) database))
         (matches (ld-find-table '(:cram :match) database)))
    (setf (ld-table-data users)
          (copy-tree
           '((2 "Bob" (1700 80)
              (:metadata :created "2026-01-01"
               :updated "2026-01-03"))
             (1 "Ada" (1400 100)
              (:metadata :created "2026-01-01"
               :updated "2026-01-02")))))
    (setf (ld-table-data problems)
          (copy-tree
           '((2 "birds-2" "Which bird?" "Raven" nil nil nil (1800 90)
              (:metadata :created "2026-01-01"
               :updated "2026-01-03"))
             (1 "plants-1" "Which plant?" "Rose" nil nil nil (1450 120)
              (:metadata :created "2026-01-01"
               :updated "2026-01-02")))))
    (setf (ld-table-data matches)
          (copy-tree
           '((2 "2026-01-04" "Raven" 1000 1.0 2 2
              (1700 80) (1800 90)
              (:metadata :created "2026-01-04"
               :updated "2026-01-04"))
             (1 "2026-01-03" "Rose" 1200 0.0 1 1
              (1400 100) (1450 120)
              (:metadata :created "2026-01-03"
               :updated "2026-01-03")))))
    database))

(ert-deftest cram-db-ui-resolves-users ()
  (let* ((*current-database* (cram-db-ui-test--database))
         (ada (cram-db-get-user "Ada")))
    (should (= (cram-user-id ada) 1))
    (should (equal (cram-db-get-user 2)
                   (car (ld-table-data* :user))))
    (should (eq (cram-db-get-user ada) ada))
    (should (= (cram-user-id* "Ada") 1))
    (should (= (cram-user-id* 7) 7))
    (should-not (cram-user-id* "missing"))))

(ert-deftest cram-db-ui-finds-latest-user-and-filters-names ()
  (let ((*current-database* (cram-db-ui-test--database)))
    (should (equal (cram-user-name (cram-db-last-user)) "Bob"))
    (should (equal (cram-db-user-names) '("Bob" "Ada")))
    (should (equal (cram-db-user-names :sans "Ada") '("Bob")))
    (should-not (cram-db-user-names :sans '("Ada" "Bob")))))

(ert-deftest cram-db-ui-resolves-problems-and-history ()
  (let ((*current-database* (cram-db-ui-test--database)))
    (should (= (cram-problem-id (cram-db-last-problem)) 2))
    (should (= (cram-problem-id (cram-db-get-problem 1)) 1))
    (should (= (length (cram-db-problems)) 2))
    (should
     (equal (cram-db-user-ratings "Ada")
            '(((1400 100) "2026-01-03"))))))

(ert-deftest cram-db-ui-filters-random-and-unused-problems ()
  (let* ((*current-database* (cram-db-ui-test--database))
         (*cram-ref-filter* "^plants-")
         (matches (ld-table :match)))
    (should (= (cram-problem-id (cram-db-random-problem)) 1))
    (should (equal (mapcar #'cram-problem-id (cram-db-problems))
                   '(1)))
    (setf (ld-table-data matches) nil)
    (should (equal (mapcar #'cram-problem-id
                           (cram-db-unused-problems))
                   '(1)))
    (setf (ld-table-data (ld-table :problem)) nil)
    (should-not (cram-db-random-problem))))

(ert-deftest cram-db-ui-weights-problems-by-match-count ()
  (let* ((*current-database* (cram-db-ui-test--database))
         (*cram-ref-filter* "")
         (matches (ld-table-data* :match)))
    (cl-letf (((symbol-function 'random-weighted-element)
               (lambda (counts inverse)
                 (should inverse)
                 (should (equal counts '((1 1) (2 1))))
                 1)))
      (should (= (cram-problem-id
                  (cram-db-random-problem
                   :matches matches :rating '(1500 350)))
                 1)))))

(ert-deftest cram-db-ui-inserts-users-and-problems ()
  (let ((*current-database* (cram-db-ui-test--database)))
    (should
     (equal (cl-subseq
             (cram-db-insert-user "Cyd" '(1500 100))
             0 3)
            '(3 "Cyd" (1500 100))))
    (should
     (equal (cl-subseq
             (cram-db-insert-problem
              '("animals-3" "Which animal?" "Fox" nil nil nil
                (1500 100)))
             0 8)
            '(3 "animals-3" "Which animal?" "Fox" nil nil nil
              (1500 100))))))

(ert-deftest cram-db-ui-reports-match-and-updates-ratings ()
  (let* ((*current-database* (cram-db-ui-test--database))
         (user (cram-db-get-user 1))
         (problem (cram-db-get-problem 1)))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _) "now"))
              ((symbol-function 'message) #'ignore))
      (pcase-let ((`(,updated-user ,updated-problem)
                   (cram-db-report-match
                    user problem "2026-11-11T15:00" "Rose" 1000 1.0
                    '(1500 90) '(1400 80))))
        (should (equal (cram-user-rating updated-user)
                       '(1500 90)))
        (should (equal (cram-problem-rating updated-problem)
                       '(1400 80)))
        (should (= (length (ld-table-data* :match)) 3))))))

(provide 'test-cram-db-ui)

;;; test-cram-db-ui.el ends here
