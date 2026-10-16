;;; test-mb-utils-physics.el --- Tests for mb-utils-physics -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-physics)

(ert-deftest mb-utils-physics-physical-constants ()
  (should (< (abs (- +gravitational-constant+ 6.67430e-11)) 1e-20))
  (should (< (abs (- +astronomical-unit+ 149597870700.0)) 0.1))
  (should (< (abs (- +parsec-in-meters+ 3.085677581e16)) 1e7)))

(ert-deftest mb-utils-physics-escape-velocity ()
  (let ((squared (escape-velocity2 +earth-radius-mean+ +earth-mass+)))
    (should (< (abs (- (sqrt squared) (escape-velocity))) 1e-12))
    (should (< 11100 (escape-velocity) 11200))
    (should (= (escape-velocity2 +earth-radius-mean+ (* 2 +earth-mass+))
	       (* 2 squared))))
  (should-error (escape-velocity 0))
  (should-error (escape-velocity 1 -1)))

(ert-deftest mb-utils-physics-parsec-conversions ()
  (should (= (parsec-to-meter) +parsec-in-meters+))
  (should (= (meter-to-parsec +parsec-in-meters+) 1.0))
  (dolist (meters '(0.0 1.0 123456789.0))
    (should (< (abs (- (parsec-to-meter (meter-to-parsec meters))
		       meters))
	       1e-7))))

(ert-deftest mb-utils-physics-unit-conversions ()
  (should (< (abs (- (J-to-cal 4.184) 1.0)) 1e-8))
  (should (< (abs (- (lbs-to-kg 1) 0.45359237)) 1e-12)))

(ert-deftest mb-utils-physics-constant-acceleration ()
  (should (= (at->s 3 2) 9.0))
  (should (= (as->t 9 2) 3.0))
  (should (= (at->v 3 2) 6))
  (should (= (as->v 9 2) 6.0))
  (should-error (as->t 1 0)))

(ert-deftest mb-utils-physics-alcohol-units ()
  (should (= (alcohol-unit 0.15 100 1) 1.0))
  (should (= (alcohol-unit :wine 750 1) 6.0))
  (should (= (alcohol-unit :ipa 330 2) 2.2))
  (should-error (alcohol-unit :unknown))
  (should-error (alcohol-unit nil)))

(provide 'test-mb-utils-physics)

;;; test-mb-utils-physics.el ends here
