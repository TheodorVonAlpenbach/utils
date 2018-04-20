(defpackage :mb-utils-test
  (:use :cl :mb-utils :mb-utils :lisp-unit))

(in-package :mb-utils-test)

(define-test test-maximum
  (flet ((m (&rest args) (multiple-value-list (apply #'maximum args))))
    (assert-equal (m '(0 1 2 3)) '(3 3 3))
    (assert-equal (m '(0 1 2 3 3)) '(3 3 3))
    (assert-equal (m '(0 1 2 3 3) :test #'>) '(0 0 0))
    (assert-equal (m '(0 1 2 3 3) :start 2) '(3 3 3))))

(setf *print-failures* t)
(run-tests :all)
