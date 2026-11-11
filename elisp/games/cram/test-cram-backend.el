;;; test-cram-backend.el --- Tests for the Cram backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cram-backend)

(defun cram-backend-test--database ()
  "Return a populated Cram database."
  (let* ((database (cram-create-database))
         (users (ld-find-table '(:cram :user) database))
         (problems (ld-find-table '(:cram :problem) database))
         (matches (ld-find-table '(:cram :match) database)))
    (setf
     (ld-table-data users)
     (copy-tree
      '((2 "Bob" (1700 80)
         (:metadata :created "2026-01-01"
          :updated "2026-01-03"))
        (1 "Ada" (1400 100)
         (:metadata :created "2026-01-01"
          :updated "2026-01-02")))))
    (setf
     (ld-table-data problems)
     (copy-tree
      '((3 "birds-3" "Third bird?" "Eagle" nil nil nil (1600 70)
         (:metadata :created "2026-01-01"
          :updated "2026-01-04"))
        (2 "birds-2" "Which bird?" "Raven" nil "Corvus corax" nil
         (1800 90)
         (:metadata :created "2026-01-01"
          :updated "2026-01-03"))
        (1 "birds-1" "First bird?" "Great (Northern) Diver"
         nil "Loon|Gavia immer" nil (1450 120)
         (:metadata :created "2026-01-01"
          :updated "2026-01-02")))))
    (setf
     (ld-table-data matches)
     (copy-tree
      '((2 "2026-01-04T12:00:00+0100" "Raven" 1000 1.0 2 2
         (1700 80) (1800 90)
         (:metadata :created "2026-01-04T12:00:00+0100"
          :updated "2026-01-04T12:00:00+0100"))
        (1 "2026-01-03T12:00:00+0100" "Loon" 1200 0.0 2 1
         (1650 90) (1450 120)
         (:metadata :created "2026-01-03T12:00:00+0100"
          :updated "2026-01-03T12:00:00+0100")))))
    database))

(defmacro cram-backend-test--with-state (&rest body)
  "Run BODY with isolated Cram backend state."
  (declare (indent 0) (debug t))
  `(let ((*current-database* (cram-backend-test--database))
         (*cram-current-user* nil)
         (*cram-current-problem* nil)
         (*cram-ref-filter* "^birds-")
         (*cram-match-cache* nil)
         (*cram-last-update* (the-creation)))
     ,@body))

(ert-deftest cram-backend-resolves-current-user ()
  (cram-backend-test--with-state
    (should (equal (cram-user-name (cram-current-user)) "Bob"))
    (should (equal (cram-user-name
                    (cram-set-current-user "Ada"))
                   "Ada"))
    (should (equal (cram-user-name (cram-current-user)) "Ada"))))

(ert-deftest cram-backend-builds-report-rows ()
  (cram-backend-test--with-state
    (cram-set-current-user "Bob")
    (let ((rows (cram-db-last-matches 2))
          (ratings (cram-db-problem-ratings)))
      (should (= (length rows) 2))
      (should
       (equal (car rows)
              '("2026-01-04T12:00:00+0100"
                "Which bird?" "Raven" "Raven" 1.0)))
      (should (equal (mapcar #'cl-fourth ratings)
                     '(1800 1600 1450))))))

(ert-deftest cram-backend-selects-unused-problem-first ()
  (cram-backend-test--with-state
    (cram-set-current-user "Bob")
    (should (= (cram-problem-id (cram-get-cram-problem)) 3))))

(ert-deftest cram-backend-selects-lowest-weighted-problem ()
  (cram-backend-test--with-state
    (cram-set-current-user "Bob")
    (setf (ld-table-data (ld-table :problem))
          (cdr (ld-table-data* :problem)))
    (cl-letf (((symbol-function 'unix-time)
               (lambda (&optional time)
                 (cond
                  ((null time) 2000)
                  ((string-match-p "2026-01-04" time) 1900)
                  (t 1000)))))
      (should (= (cram-get-cram-problem-id) 1))
      (should (= (cram-problem-id (cram-get-cram-problem)) 1)))))

(ert-deftest cram-backend-expands-and-normalizes-alternatives ()
  (cram-backend-test--with-state
    (let ((problem (cram-db-get-problem 1)))
      (should
       (member "Great Diver"
               (cram-expand-alternatives problem)))
      (should
       (member "Loon"
               (cram-expand-alternatives problem)))
      (should (cram-correct-response-p problem "loon"))
      (should (equal (cram-normalize-string "  ØRN  ") "oern")))))

(ert-deftest cram-backend-scores-correct-responses-and-time ()
  (cram-backend-test--with-state
    (let ((problem (cram-db-get-problem 2)))
      (should (= (cram-score problem "Raven" 1000) 1))
      (should (= (cram-score problem "wrong answer" 1000) 0))
      (should (= (cram-score problem "Raven" 19000) 0.5))
      (should (= (cram-score problem "Raven" 30000) 0))
      (should-error
       (cram-score problem "Raven" 1000 1000 1000)
       :type 'user-error))))

(ert-deftest cram-backend-calculates-namespaced-ratings ()
  (cram-backend-test--with-state
    (let ((user (cram-db-get-user 2))
          (problem (cram-db-get-problem 2))
          calls)
      (cl-letf (((symbol-function 'glicko-rating)
                 (lambda (player matches &optional time)
                   (push (list player matches time) calls)
                   (list (1+ (car player)) (cadr player)))))
        (should
         (equal
          (cram-glicko-new-ratings user problem 0.75 2)
          '((1701 80) (1801 90))))
        (should (= (length calls) 2))))))

(ert-deftest cram-backend-reports-response-and-updates-state ()
  (cram-backend-test--with-state
    (cram-set-current-user "Bob")
    (cram-set-current-problem (cram-db-get-problem 2))
    (cl-letf (((symbol-function 'cram-glicko-new-ratings)
               (lambda (&rest _) '((1710 75) (1790 85))))
              ((symbol-function 'format-time-string)
               (lambda (&rest _) "now"))
              ((symbol-function 'iso-date-and-time)
               (lambda (&rest _) "2026-11-11T16:00:00+0100"))
              ((symbol-function 'message) #'ignore))
      (should (= (cram-report-response "Raven" 1000) 1))
      (should (equal (cram-user-rating (cram-current-user))
                     '(1710 75)))
      (should (equal (cram-problem-rating (cram-current-problem))
                     '(1790 85)))
      (should (= (length (ld-table-data* :match)) 3)))))

(provide 'test-cram-backend)

;;; test-cram-backend.el ends here
