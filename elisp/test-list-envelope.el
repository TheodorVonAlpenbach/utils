(require 'ert)
(require 'list-envelope)

(ert-deftest test-e-make-envelope ()
  "Test of `e-make-envelope'"
  (should (equal (e-make-envelope '(0 1) '(0 1)) '((0 1) (0 1)))))

(ert-deftest test-e-envelope-p ()
  "Test of `e-envelope-p'"
  (should (e-envelope-p '((0 1) (0 1))))
  (should-not (e-envelope-p '((1 0) (0 1))))
  (should-not (e-envelope-p '((1 0))))
  (should-not (e-envelope-p '(1 0)))
  (should-not (e-envelope-p '1))
  (should-not (e-envelope-p '((0 1) nil)))
  (should-not (e-envelope-p '(nil nil)))
  (should-not (e-envelope-p nil)))

(ert-deftest test-e-contain-p ()
  "Test of `e-contain-p'"
  (should (e-contain-p '((0 3) (0 3)) '((1 2) (1 2))))
  (should (e-contain-p '((0 3) (0 3)) '((1 2) (1 2)) nil))
  (should (e-contain-p '((0 3) (0 3)) '((1 2) (1 2)) t))
  (should (e-contain-p '((0 3) (0 3)) '((1 3) (1 2))))
  (should-not (e-contain-p '((0 3) (0 3)) '((1 3) (1 2)) t)))

(ert-deftest test-e-within-p ()
  "Test of `e-within-p'"
  (should (e-contain-p '((0 3) (0 3)) '((1 2) (1 2))))
  (should (e-contain-p '((0 3) (0 3)) '((1 2) (1 2)) nil))
  (should (e-contain-p '((0 3) (0 3)) '((1 2) (1 2)) t)))

(ert-deftest test-e-touch-p ()
  "Test of `e-touch-p'"
  (should (e-touch-p '((0 3) (0 3)) '((0 3) (3 4))))
  (should-not (e-touch-p '((0 3) (0 3)) '((0 3) (2 4))))
  (should-not (e-touch-p '((0 3) (0 3)) '((0 3) (4 5))))
  (should (e-touch-p '((0 3) (0 3)) '((3 4) (0 3))))
  (should-not (e-touch-p '((0 3) (0 3)) '((2 4) (0 3))))
  (should-not (e-touch-p '((0 3) (0 3)) '((4 5) (0 3)))))

(ert-deftest test-e-overlap-p ()
  "Test of `e-overlap-p'"
  (should (e-overlap-p '((0 3) (0 3)) '((2 4) (1 3))))
  (should (e-overlap-p '((0 3) (0 3)) '((1 4) (1 3))))
  (should (e-overlap-p '((0 3) (0 3)) '((1 3) (2 4))))
  (should (e-overlap-p '((0 3) (0 3)) '((1 3) (1 4))))
  (should (e-overlap-p '((0 3) (0 3)) '((0 3) (3 4))))
  (should (e-overlap-p '((0 3) (0 3)) '((0 3) (2 4))))
  (should-not (e-overlap-p '((0 3) (0 3)) '((0 3) (4 5))))
  (should (e-overlap-p '((0 3) (0 3)) '((3 4) (0 3))))
  (should (e-overlap-p '((0 3) (0 3)) '((2 4) (0 3))))
  (should-not (e-overlap-p '((0 3) (0 3)) '((4 5) (0 3))))
  (should (e-overlap-p '((0 3) (0 3)) '((2 4) (1 3)) t))
  (should (e-overlap-p '((0 3) (0 3)) '((1 4) (1 3)) t))
  (should (e-overlap-p '((0 3) (0 3)) '((1 3) (2 4)) t))
  (should (e-overlap-p '((0 3) (0 3)) '((1 3) (1 4)) t))
  (should-not (e-overlap-p '((0 3) (0 3)) '((0 3) (3 4)) t))
  (should (e-overlap-p '((0 3) (0 3)) '((0 3) (2 4)) t))
  (should-not (e-overlap-p '((0 3) (0 3)) '((0 3) (4 5)) t))
  (should-not (e-overlap-p '((0 3) (0 3)) '((3 4) (0 3)) t))
  (should (e-overlap-p '((0 3) (0 3)) '((2 4) (0 3)) t))
  (should-not (e-overlap-p '((0 3) (0 3)) '((4 5) (0 3)) t)))

(ert-deftest test-e-disjoint-p ()
  "Test of `e-disjoint-p'"
  (should (e-disjoint-p '((0 3) (0 3)) '((4 5) (1 3))))
  (should-not (e-disjoint-p '((0 3) (0 3)) '((0 3) (3 4))))
  (should (e-disjoint-p '((0 3) (0 3)) '((0 3) (3 4)) t)))

(ert-deftest test-e-intersection ()
  "Test of `e-intersection'"
  (should (equal (e-intersection '((0 3) (0 3)) '((2 4) (1 3)))
		 '((2 3) (1 3)))))

(provide 'test-list-envelope)
