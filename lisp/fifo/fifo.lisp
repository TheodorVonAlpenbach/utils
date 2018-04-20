;;;; From https://github.com/sile/fifo/blob/master/fifo.lisp
;;;; with SBCL correction by Mats Bergstrøm

(defpackage fifo
  (:use :common-lisp)
  (:shadow push pop length)
  (:export make
	   push
	   mpush
	   pop
	   not-empty-p
	   empty-p
	   to-list
	   length))

(in-package :fifo)

(declaim (inline make push mpush pop not-empty-p empty-p to-list length)
	 (optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))

(defun make (&optional initial-contents)
  (if initial-contents
    (progn
      (assert (consp initial-contents))
      (cons (last initial-contents) initial-contents))
    (list nil)))

(defun push (x que)
  (mpush (list x) que))

(defun mpush (list que)
  (when list
    (setf (car que) (last (setf (cdr (if (not-empty-p que) (car que) que)) list))))
  que)

(defun pop (que)
  (prog1 (common-lisp:pop (cdr que))
    (setf (car que) (when (cadr que) (last que)))))

(defun not-empty-p (que)
  (cdr que))

(defun empty-p (que)
  (not (not-empty-p que)))

(defun to-list (que)
  (cdr que))

(defun length (que)
  (common-lisp:length (to-list que)))
