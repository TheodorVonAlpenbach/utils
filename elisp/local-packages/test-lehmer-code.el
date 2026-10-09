(require 'ert)
(require 'lehmer-code)

(ert-deftest test-lehmer-encode ()
  "Test of `lehmer-encode'"
  (should (equal (lehmer-encode '()) 0))
  (should (equal (lehmer-encode '(0)) 0))
  (should (equal (lehmer-encode (make-list 10 0)) 0))
  (should (equal (lehmer-encode '(1)) 1))
  (should (equal (lehmer-encode '(0 1)) 1))
  (should (equal (lehmer-encode '(1 0)) 2))
  (should (equal (lehmer-encode '(2 2 1)) 17)))

(ert-deftest test-lehmer-deencode ()
  "Test of `lehmer-encode'"
  (cl-loop for i below 24
	   do (should (equal (lehmer-encode (lehmer-decode i)) i)))
  (cl-loop for i from 2 to 100
	   for n = (expt 10 i)
	   do (should (equal (lehmer-encode (lehmer-decode n)) n))))

(ert-deftest test-permutation-deencode ()
  "Test of `lehmer-encode'"
  (cl-loop for i below 24
	   do (should (equal (encode-permutation (decode-permutation i)) i)))
  (cl-loop for i from 2 to 100
	   for n = (expt 10 i)
	   do (should (equal (encode-permutation (decode-permutation n)) n))))

(provide 'test-lehmer-code)
