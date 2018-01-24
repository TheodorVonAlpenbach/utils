;;;; See https://github.com/OdonataResearchLLC/lisp-unit/wiki for a
;;;; description of LISP-UNIT.

(defpackage :test-elapsed
  (:use :cl :local-time :elapsed :lisp-unit))

(in-package :test-elapsed)

(enable-read-macros)

(define-test test-elapsed
  (assert-equal (elapsed @2018-01-16T12:58:00 :sec @2018-01-16T12:58:00) 0)
  (assert-equal (elapsed @2018-01-16T12:58:10 :sec @2018-01-16T12:58:00) 10)
  (assert-equal (elapsed @2018-02-16T12:00:00 :day @2018-01-16T12:58:00) 31)
  (assert-equal (elapsed @2018-03-16T12:00:00 :week @2018-02-16T12:00:00) 4)
  (assert-equal (elapsed @2019-03-16T12:00:00 :month @2018-02-16T12:00:00) 13)
  (assert-equal (elapsed @2018-01-17T12:58:00 :year @2018-01-16T12:58:00) 1)
  (assert-equal (elapsed @2018-01-26T12:58:00 :decade @2018-01-16T12:58:00) 1)
  (assert-equal (elapsed @2118-01-16T12:58:00 :century @2018-01-16T12:58:00) 1)
  (assert-equal (elapsed @3018-01-16T12:58:00 :millennium @2018-01-16T12:58:00) 1)
  (assert-error 'simple-error (elapsed @2018-01-16T12:58:00 :qwe)))

(timestamp-whole-year-difference @2018-02-17T11:00:00 @2017-02-17T12:00:00)
(elapsed @2018-02-17T11:00:00 :year @2017-02-17T12:00:00)

(define-test test-iso-week-number-of
  (assert-equal (iso-week-number-of @2018-01-16T12:58:00) 3))

(setf *print-failures* t)
(print-errors (run-tests '(test-iso-week-number-of)))
(print-errors (run-tests '(test-elapsed)))
