;;; test-mb-combinatorics.el --- Tests for mb-combinatorics -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-combinatorics)

(ert-deftest mb-combinatorics-fibonacci-numbers ()
  "Test of `fibonacci-numbers'"
  (should (equal (fibonacci-numbers 0) nil))
  (should (equal (fibonacci-numbers 1) '(0)))
  (should (equal (fibonacci-numbers 2) '(0 1)))
  (should (equal (fibonacci-numbers 3) '(0 1 1)))
  (should (equal (fibonacci-numbers 10) '(0 1 1 2 3 5 8 13 21 34)))
  (should (equal (fibonacci-numbers 8 '(1 2))
		 (nthcdr 2 (fibonacci-numbers 10)))))

(ert-deftest mb-combinatorics-fibonacci-nth ()
  (should (< (abs (- (fibonacci-nth 10) 55)) 1e-10)))

(ert-deftest mb-combinatorics-factorial ()
  (should (equal (mapcar #'factorial (0-n 7)) '(1 1 2 6 24 120 720)))
  (should (= (factorial 5 3) 60))
  (should-error (factorial -1))
  (should-error (derangement -1)))

(ert-deftest mb-combinatorics-catalan-nth ()
  "Test of `catalan-nth'"
  (should (equal (mapcar #'catalan-nth (0-n 10))
		 '(1 1 2 5 14 42 132 429 1430 4862))))

(ert-deftest mb-combinatorics-aliquot-sum ()
  "Test of `aliquot-sum'"
  (should (equal (mapcar #'aliquot-sum (1-n 10)) '(0 1 1 3 1 6 1 7 4 8)))
  (should (equal (mapcar #'aliquot-sum '(1 6 28 220 284)) '(0 6 28 284 220))))

(ert-deftest mb-combinatorics-amicable-numbers ()
  (should (amicable-numbers-p 220 284))
  (should-not (amicable-numbers-p 6 6))
  (should-not (amicable-numbers-p 220 220)))

(ert-deftest mb-combinatorics-derangement ()
  "Test of `derangement'"
  (should (equal (mapcar #'derangement (0-n 11))
		 '(1 0 1 2 9 44 265 1854 14833 133496 1334961))))

(ert-deftest mb-combinatorics-binomial-coefficient ()
  "Test of `binomial-coefficient'"
  (should (equal (binomial-coefficient 5 0) 1))
  (should (equal (binomial-coefficient 5 -1) 0))
  (should (equal (binomial-coefficient 5 6) 0))
  (should (equal (mapcar (lambda (m) (binomial-coefficient 5 m))
			 (a-b 0 5))
		 '(1 5 10 10 5 1))))

(ert-deftest mb-combinatorics-stirling-numbers-first-kind ()
  "Test of `stirling-numbers-1'"
  (should (equal (cl-loop for k to 5 collect (stirling-numbers-1 5 k))
		 '(0 24 50 35 10 1))))

(ert-deftest mb-combinatorics-stirling-numbers-second-kind ()
  (should (equal (cl-loop for k to 5 collect (stirling-numbers-2 5 k))
		 '(0 1 15 25 10 1))))

(ert-deftest mb-combinatorics-uefa-opponents ()
  (should (equal (uefa-opposite-team-group 'Barcelona)
		 (uefa-runner-ups)))
  (should (equal (uefa-opposite-team-group 'Porto)
		 (uefa-winners)))
  (should (equal (uefa-possible-opponents-gen 'Barcelona)
		 '(Porto Arsenal Milan Shakhtar-Donetsk Galatasaray)))
  (should-not (uefa-possible-opponents-gen-p 'Porto 'Arsenal)))

(ert-deftest mb-combinatorics-uefa-draw-recursion ()
  (let ((groups '(((A x) (B y))
		  ((C z) (D w)))))
    (should (equal (uefa-possible-draws-main groups)
		   '(((A D) ((C B) t)))))))

(ert-deftest mb-combinatorics-rummy-hands ()
  (should (= (rummy-hands 2) 11639131735604403030))
  (should-error (rummy-hands 6 10 52)))

(provide 'test-mb-combinatorics)

;;; test-mb-combinatorics.el ends here
