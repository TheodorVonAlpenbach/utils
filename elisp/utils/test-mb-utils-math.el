(require 'ert)
(require 'mb-utils-math)

(ert-deftest test-generate-addends-fixed ()
  "Test of `generate-addends-fixed'"
  (should (equal (generate-addends-fixed 577 51)
		 (cl-destructuring-bind (q r) (cl-floor 577 51)
		   (append (make-list r (1+ q))
			   (make-list (- 51 r) q))))))

(ert-deftest test-unsignum ()
  "Test of `unsignum'"
  (should (equal (mapcar #'unsignum '(-2 0 2)) '(1 0 -1)))
  (should (equal (unsignum 1 -1) 1)))

(ert-deftest test-uint-to-n-base ()
  "Test of `uint-to-n-base'"
  (should (equal (cl-loop for i in (a-b 99 101) collect (uint-to-n-base i))
		 '((9 9) (1 0 0) (1 0 1))))
  (should (equal (cl-loop for i in '(0 1 9 10) collect (uint-to-n-base i))
		 '(() (1) (9) (1 0))))
  (should (equal (cl-loop for i in '(0 1 9 10) collect (uint-to-n-base i 10 2))
		 '((0 0) (0 1) (0 9) (1 0))))
  (should (equal (cl-loop for i in '(0 1 2 3 4) collect (uint-to-n-base i 2))
		 '(() (1) (1 0) (1 1) (1 0 0))))
  (should (equal (cl-loop for i in '(0 1 2 3 4) collect (uint-to-n-base i 2 3))
		 '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0)))))

(ert-deftest test-uint-length ()
  "Test of `uint-length'"
  (should (equal (mapcar #'uint-length (0-n 11))
		 '(0 1 1 1 1 1 1 1 1 1 2)))
  (should (equal (uint-length (0-n 11)) 2)))

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

(ert-deftest test-floor-to ()
  "Test of `floor-to'"
  (should (equal (floor-to 70 25) 50))
  (should (equal (floor-to 75 25) 75)))

(ert-deftest test-ceiling-to ()
  "Test of `ceiling-to'"
  (should (equal (ceiling-to 70 25) 75))
  (should (equal (ceiling-to 75 25) 75)))

(ert-deftest test-next-greater-multiple ()
  "Test of `next-greater-multiple'"
  (should (equal (next-greater-multiple 4 5) 5))
  (should (equal (next-greater-multiple 5 5) 10))
  (should (equal (next-greater-multiple 6 5) 10)))

(ert-deftest test-next-smaller-multiple ()
  "Test of `next-smaller-multiple'"
  (should (equal (next-smaller-multiple 4 5) 0))
  (should (equal (next-smaller-multiple 5 5) 0))
  (should (equal (next-smaller-multiple 6 5) 5)))

(defun test-random-weighted-element-1 (weighted-elements inverse)
  (abs (modb
	(vec-angle
	 (if inverse
	   (mapcar (bind #'inv t) (project-sequence weighted-elements 1))
	   (project-sequence weighted-elements 1))
	 (project-sequence
	  (accumulate-list
	   (cl-loop repeat 1000
		 collect (random-weighted-element weighted-elements inverse))
	   :test #'symbol<)
	  1))
	2pi (- pi))))
;;(test-random-weighted-element-1 '((a 1) (b 5) (c 10)) t)

(ert-deftest test-random-weighted-element ()
  "Test of `random-weighted-element'"
  (should (< (test-random-weighted-element-1 '((a 1) (b 5) (c 10)) nil) .1))
  (should (< (test-random-weighted-element-1 '((a 1) (b 5) (c 10)) 1) .1)))

(ert-deftest test-area-secant ()
  "Test of `area-secant'"
  (should (equal (area-secant 0) 0.0))
  (should (equal (area-secant 1) 0.07926450759605175))
  (should (equal (area-secant float-pi) (/ float-pi 2))))

(provide 'test-mb-utils-math)
