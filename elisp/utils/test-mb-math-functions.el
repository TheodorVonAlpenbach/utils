(require 'ert)
(require 'mb-math-functions)

(ert-deftest test-geometric-series ()
  "Test of `geometric-series'"
  (should (equal (geometric-series 0.5) 2.0))
  (should (equal (geometric-series 0.5 :end 2) 1.5))
  (should (equal (geometric-series 0.5 :end 3) 1.75))
  (should (equal (geometric-series 0.5 :start 1 :end 3) 0.75)))

(ert-deftest test-falsi-method ()
  "Test of `falsi-method'"
  (should (< (abs (- (falsi-method
		      #'(lambda (x) (- (cos x) (expt x 3))) 0 1 5E-15 100)
		     0.865474033101614))
	     5E-15)))

(provide 'test-mb-math-functions)
