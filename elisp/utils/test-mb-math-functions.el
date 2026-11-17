;;; test-mb-math-functions.el --- Tests for mb-math-functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-math-functions)

(defun mb-math-functions-test--close-p (actual expected &optional tolerance)
  "Return non-nil when ACTUAL is close to EXPECTED."
  (< (abs (- actual expected)) (or tolerance 1e-10)))

(ert-deftest mb-math-functions-degree-and-hyperbolic-functions ()
  "Test generated degree functions and hyperbolic identities."
  (should (mb-math-functions-test--close-p (cosd 60) 0.5))
  (should (mb-math-functions-test--close-p (asind 0.5) 30))
  (should (mb-math-functions-test--close-p (asecd 2) 60))
  (should (mb-math-functions-test--close-p
           (tanh 1)
           (/ (sinh 1) (cosh 1)))))

(ert-deftest mb-math-functions-geometric-series ()
  "Test finite and infinite geometric sums and their index domains."
  (should (equal (geometric-series 0.5) 2.0))
  (should (equal (geometric-series 0.5 :end 2) 1.5))
  (should (equal (geometric-series 0.5 :end 3) 1.75))
  (should (equal (geometric-series 0.5 :start 1 :end 3) 0.75))
  (should (equal (geometric-series 0.5 :start 2) 0.5))
  (should (equal (geometric-series 1 :start 3 :end 2) 0))
  (should (equal (geometric-series -1 :end 4) 0.0))
  (should-error (geometric-series 1))
  (should-error (geometric-series 0.5 :start -1))
  (should-error (geometric-series 0.5 :end 1.5)))

(ert-deftest mb-math-functions-atan2-and-quadratic-root ()
  "Test quadrant handling and quadratic roots."
  (should (mb-math-functions-test--close-p
           (atan2 -1 1) (* 0.75 float-pi)))
  (should (eq (atan2 0 0) 'indeterminate))
  (should (equal (quadratic-root 1 0 -1) '(1.0 -1.0)))
  (should-error (quadratic-root 1 0 1))
  (should-error (quadratic-root 0 2 1)))

(ert-deftest mb-math-functions-error-and-dilog-functions ()
  "Test numerical special functions at known values."
  (should (mb-math-functions-test--close-p
           (erf 1) 0.8427007929497149 2e-7))
  (should (mb-math-functions-test--close-p (erfc 1) (- 1 (erf 1))))
  (should (mb-math-functions-test--close-p
           (dilog 1) (/ (* float-pi float-pi) 6)))
  (should (= (normal-distribution-quantile 0.5) 0.0))
  (should-error (polylogarithm-abs-lt-1 1))
  (should-error (inverse-erf 2))
  (should-error (normal-distribution-quantile 0))
  (should-error (normal-distribution-quantile 0.5 0 0)))

(ert-deftest mb-math-functions-falsi-method ()
  "Test false-position convergence, endpoints, and interval validation."
  (should (< (abs (- (falsi-method
		      #'(lambda (x) (- (cos x) (expt x 3))) 0 1 5E-15 100)
		     0.865474033101614))
	     5E-15))
  (should (= (falsi-method #'identity 0 1 1e-6 10) 0))
  (should-error
   (falsi-method (lambda (x) (+ (* x x) 1)) -1 1 1e-6 10))
  (should-error (falsi-method #'identity 1 0 1e-6 10)))

(ert-deftest mb-math-functions-newton-raphson ()
  "Test Newton-Raphson convergence and result metadata."
  (pcase-let ((`(,root ,scaled-error ,iterations ,partial)
               (newton-raphson
                (lambda (x) (- (* x x) 2))
                (lambda (x) (* 2 x))
                1.0)))
    (should (mb-math-functions-test--close-p root (sqrt 2)))
    (should (< (abs scaled-error) 1))
    (should (= iterations (1- (length partial)))))
  (should-error (newton-raphson #'identity (lambda (_) 0) 1.0))
  (should-error (newton-raphson #'identity #'identity 1.0 0)))

(provide 'test-mb-math-functions)

;;; test-mb-math-functions.el ends here
