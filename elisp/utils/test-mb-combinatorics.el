(require 'ert)
(require 'mb-combinatorics)

(ert-deftest test-fibonacci-numbers ()
  "Test of `fibonacci-numbers'"
  (should (equal (fibonacci-numbers 0) nil))
  (should (equal (fibonacci-numbers 1) '(0)))
  (should (equal (fibonacci-numbers 2) '(0 1)))
  (should (equal (fibonacci-numbers 3) '(0 1 1)))
  (should (equal (fibonacci-numbers 10) '(0 1 1 2 3 5 8 13 21 34)))
  (should (equal (fibonacci-numbers 8 '(1 2))
		 (nthcdr 2 (fibonacci-numbers 10)))))

(ert-deftest test-catalan-nth ()
  "Test of `catalan-nth'"
  (should (equal (mapcar #'catalan-nth (0-n 10))
		 '(1 1 2 5 14 42 132 429 1430 4862))))

(provide 'test-mb-combinatorics)
