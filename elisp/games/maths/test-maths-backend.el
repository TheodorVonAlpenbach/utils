;;; test-maths-backend.el --- Tests for the Maths backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'maths-backend)

(ert-deftest maths-backend-has-glicko-defaults ()
  (should (= +maths-default-rating+ 1500))
  (should (= +maths-default-RD+ 350))
  (should (= +maths-default-rating-window+ 200))
  (should (equal +maths-default-user-name+ "Mats")))

(ert-deftest maths-backend-accesses-user-and-task-records ()
  (let* ((metadata '(:metadata :created "then" :updated "now"))
         (user (list 1 "Ada" 8 1500 250 metadata))
         (task (list 2 :addition 1 '(3 4) 7 1400 200 metadata)))
    (should (= (maths-user-id user) 1))
    (should (equal (maths-user-name user) "Ada"))
    (should (= (maths-user-age user) 8))
    (should (= (maths-user-rating user) 1500))
    (should (= (maths-user-RD user) 250))
    (should (equal (maths-user-last-updated user) "now"))
    (should (= (maths-task-id task) 2))
    (should (eq (maths-task-operation task) :addition))
    (should (equal (maths-task-arguments task) '(3 4)))
    (should (= (maths-task-solution task) 7))
    (should (= (maths-task-rating task) 1400))
    (should (= (maths-task-RD task) 200))))

(ert-deftest maths-backend-calculates-supported-operations ()
  (should (= (maths-calculate :addition '(8 2)) 10))
  (should (= (maths-calculate :substraction '(8 2)) 6))
  (should (= (maths-calculate :multiplication '(8 2)) 16))
  (should (= (maths-calculate :division '(8 2)) 4)))

(ert-deftest maths-backend-generates-valid-division-arguments ()
  (dotimes (_ 50)
    (pcase-let ((`(,numerator ,denominator)
                 (maths-arguments 3 :division)))
      (should-not (zerop denominator))
      (should (zerop (% numerator denominator))))))

(ert-deftest maths-backend-estimates-task-levels ()
  (should (= (maths-estimate-level '(nil nil nil (3 4))) 1))
  (should (= (maths-estimate-level '(nil nil nil (8 9))) 2))
  (should (= (maths-estimate-level '(nil nil nil (30 40))) 3))
  (should (= (maths-estimate-level '(nil nil nil (300 400))) 4)))

(ert-deftest maths-backend-scores-answers-and-time ()
  (let ((task '(1 :addition 1 (3 4) 7 1500 350)))
    (should (= (maths-score task 6 1000) 0))
    (should (= (maths-score task 7 1000) 1))
    (should (= (maths-score task 7 20000) 0))
    (should (= (maths-score task 7 11000) 0.5))))

(ert-deftest maths-backend-uses-namespaced-rating-helper ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'glicko-rating)
               (lambda (player matches &optional time)
                 (push (list player matches time) calls)
                 (list (+ (car player) 1) (cadr player)))))
      (should
       (equal
        (maths-glicko-new-ratings
         '(1 "Ada" 8 1500 250)
         '(2 :addition 1 (3 4) 7 1400 200)
         0.75
         2)
        '((1501 250) (1401 200))))
      (should (= (length calls) 2)))))

(ert-deftest maths-backend-returns-top-rating-range ()
  (let* ((database (maths-create-database))
         (*current-database* database)
         (users (ld-find-table '(:maths :users) database)))
    (setf (ld-table-data users)
          '((1 "Ada" 8 1400 100)
            (2 "Bob" 9 1700 100)
            (3 "Cyd" 10 1500 100)))
    (should
     (equal (mapcar #'maths-user-name
                    (maths-top-ratings :users :from 0 :to 2))
            '("Bob" "Cyd")))
    (should-error
     (maths-top-ratings :unknown)
     :type 'user-error)
    (should-error
     (maths-top-ratings :users :from 2 :to 1)
     :type 'user-error)))

(provide 'test-maths-backend)

;;; test-maths-backend.el ends here
