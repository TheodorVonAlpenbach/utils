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

(ert-deftest test-aliquot-sum ()
  "Test of `aliquot-sum'"
  (should (equal (mapcar #'aliquot-sum (1-n 10)) '(0 1 1 3 1 6 1 7 4 8)))
  (should (equal (mapcar #'aliquot-sum '(1 6 28 220 284)) '(0 6 28 284 220))))

(ert-deftest test-derangement ()
  "Test of `derangement'"
  (should (equal (mapcar #'derangement (0-n 11))
		 '(1 0 1 2 9 44 265 1854 14833 133496 1334961))))

(ert-deftest test-binomial-coefficient ()
  "Test of `binomial-coefficient'"
  (should (equal (binomial-coefficient 5 0) 1))
  (should (equal (binomial-coefficient 5 -1) 0)))

(ert-deftest test-stirling-numbers-1 ()
  "Test of `stirling-numbers-1'"
  (should (equal (loop for k to 5 collect (stirling-numbers-1 5 k))
		 '(0 24 50 35 10 1))))

(provide 'test-mb-combinatorics)
