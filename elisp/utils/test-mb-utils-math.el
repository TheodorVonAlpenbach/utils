(require 'ert)
(require 'mb-utils-math)

(ert-deftest test-generate-addends-fixed ()
  "Test of `generate-addends-fixed'"
  (should (equal (generate-addends-fixed 577 51)
		 (destructuring-bind (q r) (cl-floor 577 51)
		   (append (make-list r (1+ q))
			   (make-list (- 51 r) q))))))

(ert-deftest test-unsignum ()
  "Test of `unsignum'"
  (should (equal (mapcar #'unsignum '(-2 0 2)) '(1 0 -1)))
  (should (equal (unsignum 1 -1) 1)))

(ert-deftest test-uint-to-n-base ()
  "Test of `uint-to-n-base'"
  (should (equal (loop for i in (a-b 99 101) collect (uint-to-n-base i))
		 '((9 9) (1 0 0) (1 0 1))))
  (should (equal (loop for i in '(0 1 9 10) collect (uint-to-n-base i))
		 '(() (1) (9) (1 0))))
  (should (equal (loop for i in '(0 1 9 10) collect (uint-to-n-base i 10 2))
		 '((0 0) (0 1) (0 9) (1 0))))
  (should (equal (loop for i in '(0 1 2 3 4) collect (uint-to-n-base i 2))
		 '(() (1) (1 0) (1 1) (1 0 0))))
  (should (equal (loop for i in '(0 1 2 3 4) collect (uint-to-n-base i 2 3))
		 '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0)))))

(ert-deftest test-uint-length ()
  "Test of `uint-length'"
  (should (equal (mapcar #'uint-length (0-n 11))
		 '(0 1 1 1 1 1 1 1 1 1 2)))
  (should (equal (uint-length (0-n 11)) 2)))

(ert-deftest test-L-sum ()
  "Test of `L-sum'"
 (should (equal (L-sum ) nil)))

(ert-deftest test-cumsum ()
  "Test of `cumsum'"
 (should (equal (cumsum (0-n 3)) '(0 1 3)))
 (should (equal (cumsum (0-n 3) :initial-value 2) '(2 3 5)))
 (should (equal (cumsum (1-n 3) :key #'* :initial-value 1) '(1 2 6)))
 (should (equal (cumsum (coerce (0-n 3) 'vector)) [0 1 3])))

(ert-deftest test-sum ()
  "Test of `sum'"
  (should (equal (sum '(1 2 3 4) :start 1 :initial-value 1) 10))
  (should (equal (sum '(1 2 3 4) :operator #'+ :start 1 :initial-value 1) 10))
  (should (equal (sum '(1 2 3 4) :operator #'* :start 1 :initial-value 1) 24)))

(provide 'test-mb-utils-math)
