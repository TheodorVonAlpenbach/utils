;;; test-glicko.el --- Tests for Glicko-1 ratings -*- lexical-binding: t; -*-

(require 'ert)
(require 'glicko)

(defun glicko-test--approximately= (left right tolerance)
  "Return non-nil when LEFT is within TOLERANCE of RIGHT."
  (<= (abs (- left right)) tolerance))

(ert-deftest glicko-has-standard-defaults ()
  (should (equal +glicko-init-rating+ '(1500 350)))
  (should (= +glicko-max-RD+ 350))
  (should (> +glicko-q-constant+ 0)))

(ert-deftest glicko-restores-and-caps-rating-deviation ()
  (should (= (glicko-update-RD 50) 50.0))
  (should
   (glicko-test--approximately=
    (glicko-update-RD 50 +glicko-RD-restore-time+)
    350.0 0.000001))
  (should (= (glicko-update-RD 350 10) 350.0))
  (should-error (glicko-update-RD -1) :type 'user-error)
  (should-error (glicko-update-RD 50 -1) :type 'user-error))

(ert-deftest glicko-expected-scores-are-symmetric ()
  (let ((left (glicko-expected-score 1500 1400 30))
        (right (glicko-expected-score 1400 1500 30)))
    (should (glicko-test--approximately=
             (+ left right) 1.0 0.000001))
    (should (> left 0.5))))

(ert-deftest glicko-matches-published-example ()
  (pcase-let ((`(,rating ,RD)
               (glicko-rating
                '(1500 200)
                '(((1400 30) 1)
                  ((1550 100) 0)
                  ((1700 300) 0)))))
    (should (glicko-test--approximately=
             rating 1464.06 0.1))
    (should (glicko-test--approximately=
             RD 151.4 0.1))))

(ert-deftest glicko-accepts-single-or-list-match ()
  (let ((single
         (glicko-rating '(1500 200) '((1400 30) 1)))
        (list-form
         (glicko-rating '(1500 200) '(((1400 30) 1)))))
    (should (equal single list-form))))

(ert-deftest glicko-inflates-inactive-player-without-matches ()
  (pcase-let ((`(,rating ,RD)
               (glicko-rating '(1500 50) nil 10)))
    (should (= rating 1500.0))
    (should (> RD 50))
    (should (<= RD +glicko-max-RD+))))

(ert-deftest glicko-rejects-invalid-input ()
  (should-error
   (glicko-rating '(1500 -1) '((1400 30) 1))
   :type 'user-error)
  (should-error
   (glicko-rating '(1500 100) '((1400 30) 2))
   :type 'user-error)
  (should-error
   (glicko-rating '(1500 100) '((1400 30)))
   :type 'user-error))

(provide 'test-glicko)

;;; test-glicko.el ends here
