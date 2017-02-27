(defpackage :timeseries-test
  (:use :cl :mb-utils :timeseries :csv :lisp-unit))

(in-package :timeseries-test)

(defvar *test-path* "/home/mbe/projects/utils/lisp/timeseries/random-walks.csv")
(defvar *fasit-path* "/home/mbe/projects/utils/lisp/timeseries/random-walks.dat")
(defvar *test-threshold* 2)

(defun parse-double (s)
  "Shorthand for parsing a double"
  (parse-number:parse-real-number s :float-format 'double-float))

(defun read-test-data (&optional (path *test-path*))
  "Read csv data and convert them to a 2d lisp tree."
  (maptree #'parse-double (read-csv-file path :column-separator #\;)))
;;(read-test-data)

(defun tss-test-data (&optional (path *test-path*))
  "Return data in PATH as a list of timeseries"
  (let ((tss (transpose-tree (read-test-data path))))
    (mapcar (compose #'transpose-tree (bind #'list (first tss) 1)) (rest tss))))
;;(tss-test-data)
;;(turning-points (subseq (first (tss-test-data)) 0) 2 :key #'second)
;;(turning-points (mapcar #'second (subseq (first (tss-test-data)) 19 50)) 2)
(mapcar #'second (subseq (first (tss-test-data)) 19 300))
(turning-points (mapcar #'second (subseq (first (tss-test-data)) 50 120)) 2 :debug t)
(subseq (first (read-tp-fasit)) 20 30)


(defun setfsecond (x y) (setf (second x) y))

(defun tp-test-data (&optional (path *test-path*))
  "Read test timeseries and calculate their turning points."
  (loop for ts in (tss-test-data path)
	collect (turning-points ts *test-threshold*
				:key #'second
				:setkey #'setfsecond)))
;;(tp-test-data)

(defun record-tp-test-data (&optional (path *test-path*))
  "Records the result of the current turning-points function.
Use this with caution! This should be only when

   * an error in turning-points has been corrected
   * a new output format is required (while turning-points is unaltered)

(Of course, you could run it if nothing has been changed, but why
should you want to do that?!)"
  (with-open-file (s *fasit-path* :direction :output :if-exists :supersede)
    (print (tp-test-data path) s)))
;;(record-tp-test-data)

(defun read-tp-fasit (&optional (path *fasit-path*))
  "Read the turning point fasit into a list of TP timeseries.
See record-tp-test-data for info on the creation of the fasit."
  (with-open-file (s path)
    (read s)))
;;(read-tp-fasit)

(defun clean-ts (ts amplitude &key (key #'abs))
  "Remove start and end zigzag patterns from timeseries TS"
  (flet ((clean-front (x) (member amplitude x :key key :test #'/=)))
    (nreverse (clean-front (reverse (clean-front ts))))))
;;(clean-ts '(3 -3 3 -3 -3 0 0 0 -3 3 -3) 3)

(defun clean-fasit (fasit amplitude &optional (key #'(lambda (x) (abs (second x)))))
  "Remove start and end zigzag patterns from timeseries in FASIT"
  (loop for ts in fasit collect (clean-ts ts amplitude :key key)))
;;(length (first (clean-fasit (read-tp-fasit) 100)))

;; template for creating an octave format
;; (write-list (subseq (second (read-tp-fasit)) 20 294) t
;;   :key #'(lambda (x) (format nil "~f, ~f" (first x) (second x)))
;;   :pre "[ " :in :newline :suf " ]")

(define-test test-tp-test-data
  (assert-equal 2 (length (tp-test-data)))
  (assert-equalp  (clean-fasit (tp-test-data) 100)))
;;(tp-test-data)

(run-tests :all)

(loop for (t1 x1) in (first (clean-fasit (read-tp-fasit) 100))
      for (t2 x2) in (first (clean-fasit (tp-test-data) 100))
      for dx = (- x1 x2)
      collect (list x1 x2 dx))
