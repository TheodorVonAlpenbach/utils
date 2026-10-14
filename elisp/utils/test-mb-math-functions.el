;;; test-mb-math-functions.el --- Tests for mb-math-functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-math-functions)

(defun test-mb-math-functions--close-p (actual expected &optional tolerance)
  "Return non-nil when ACTUAL is close to EXPECTED."
  (< (abs (- actual expected)) (or tolerance 1e-10)))

(ert-deftest test-degree-and-hyperbolic-functions ()
  "Test generated degree functions and hyperbolic identities."
  (should (test-mb-math-functions--close-p (cosd 60) 0.5))
  (should (test-mb-math-functions--close-p (asind 0.5) 30))
  (should (test-mb-math-functions--close-p (asecd 2) 60))
  (should (test-mb-math-functions--close-p
           (tanh 1)
           (/ (sinh 1) (cosh 1)))))

(ert-deftest test-geometric-series ()
  "Test of `geometric-series'"
  (should (equal (geometric-series 0.5) 2.0))
  (should (equal (geometric-series 0.5 :end 2) 1.5))
  (should (equal (geometric-series 0.5 :end 3) 1.75))
  (should (equal (geometric-series 0.5 :start 1 :end 3) 0.75))
  (should (equal (geometric-series -1 :end 4) 0.0))
  (should-error (geometric-series 1)))

(ert-deftest test-atan2-and-quadratic-root ()
  "Test quadrant handling and quadratic roots."
  (should (test-mb-math-functions--close-p
           (atan2 -1 1) (* 0.75 float-pi)))
  (should (eq (atan2 0 0) 'indeterminate))
  (should (equal (quadratic-root 1 0 -1) '(1.0 -1.0)))
  (should-error (quadratic-root 1 0 1)))

(ert-deftest test-error-and-dilog-functions ()
  "Test numerical special functions at known values."
  (should (test-mb-math-functions--close-p
           (erf 1) 0.8427007929497149 2e-7))
  (should (test-mb-math-functions--close-p (erfc 1) (- 1 (erf 1))))
  (should (test-mb-math-functions--close-p
           (dilog 1) (/ (* float-pi float-pi) 6)))
  (should (= (normal-distribution-quantile 0.5) 0.0)))

(ert-deftest test-falsi-method ()
  "Test of `falsi-method'"
  (should (< (abs (- (falsi-method
		      #'(lambda (x) (- (cos x) (expt x 3))) 0 1 5E-15 100)
		     0.865474033101614))
	     5E-15)))

(ert-deftest test-newton-raphson ()
  "Test Newton-Raphson convergence and result metadata."
  (pcase-let ((`(,root ,scaled-error ,iterations ,partial)
               (newton-raphson
                (lambda (x) (- (* x x) 2))
                (lambda (x) (* 2 x))
                1.0)))
    (should (test-mb-math-functions--close-p root (sqrt 2)))
    (should (< (abs scaled-error) 1))
    (should (= iterations (1- (length partial))))))

(provide 'test-mb-math-functions)

;;; test-mb-math-functions.el ends here
