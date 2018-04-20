(defpackage test-fifo
  (:use :cl :fifo :lisp-unit)
  (:shadow push pop length))

(in-package :test-fifo)

(define-test test-make
  (assert-equal (make) '(nil))
  (assert-equal (make '(1)) '((1) 1)))

(define-test test-empty-p
  (assert-true (empty-p (make))))

(define-test test-push
  (assert-equal (fifo:push 1 (make)) '((1) 1)))

(define-test test-mpush
  (assert-equal (fifo:mpush '(a b c) (make)) '((c) a b c)))

(define-test test-pull
  (let ((q (make '(1))))
    (assert-equal (fifo:pop q) 1)
    (assert-true (empty-p q))
    (assert-false (fifo:pop q))
    (assert-true (empty-p q))))

(define-test test-to-list
  (assert-equal (to-list (make '(a b c))) '(a b c)))

(define-test test-length
  (assert-equal (fifo:length (make '(a b c))) 3))

;;(run-tests '(test-distance))
(setf *print-failures* t)
(run-tests :all)
;;(remove-tests '(test-left-p))
