;;;; See https://github.com/OdonataResearchLLC/lisp-unit/wiki/Reference
;;;; for info about :lisp-unit

(defpackage :topology-test
  (:use :cl :mb-utils :topology :lisp-unit))

(in-package :topology-test)

(defun mp (x1 y1) (make-point (list x1 y1)))
(defun ms (x1 y1 x2 y2) (make-segment `(,x1 ,y1) `(,x2 ,y2)))
;;(ms 0 0 1 0)

(defun mss (&rest xs-and-ys)
  (loop for (s e) in (pairs (cut xs-and-ys))
	collect (make-segment s e)))
;;(mss 0 0 1 0 0 0 0 1)
(defun mpa (&rest xs-and-ys) (make-path (cut xs-and-ys)))
(defun mpl (&rest xs-and-ys) (make-path (cut xs-and-ys)))
(defun mpg (&rest xs-and-ys) (make-polygon (cut xs-and-ys)))

(define-test test-make-geometries
  (let ((p0 (make-point '(0 0)))
	(p1 (make-point '(1 0))))
    (assert-equal 'point (type-of p1))
    (assert-equal 'segment (type-of (make-segment p0 p1)))
    (assert-equal 'path (type-of (make-path (list p0 p1))))))

(define-test test-coordinates
  (assert-equal '(1 0) (coordinates (make-point '(1 0)))))

(define-test test-area
  (assert-equalp 6 (area (mpg 0 0  1 0  2 1  3 0  3 2  2 3  1 2  0 2)))
  (assert-equalp 17 (area (make-ellipse (ms -17 0 17 0) (/ 2 pi))))
  (assert-equalp 15 (area (make-box 0 3 0 5))))

;;(run-tests '(test-make-geometries test-coordinates))
(run-tests :all)
;;(setf *print-failures* t)
;;(remove-tests '(test-make-point))
