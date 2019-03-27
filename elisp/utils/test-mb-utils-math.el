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

(provide 'test-mb-utils-math)
