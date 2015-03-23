(defpackage :mb-gnuplot
  (:nicknames :gp)
  (:use :common-lisp :mb-utils)
  (:export :plot))

(in-package :mb-gnuplot)

;;; Move this to utils later
(defmacro with-temporary-file ((stream &optional (prefix "/tmp/tmp")) &rest body)
  "Executes BODY with STREAM bound to a file stream to a newly created
file. Returns the pathname of the stream together with the value of last form in BODY."
  `(let ((,stream (posix:mkstemp ,prefix)))
     (values (pathname ,stream)
	     (prog1 (progn ,@body) (close ,stream)))))

(defun write-function (stream function &key x-values (resolution 100))
  "Returns the filename containing RESOLUTION samples of FUNCTION in the interval X-VALUES.
TODO: Handle n-ary functions and generalize X-VALUES to N dimensions."
  (loop with (a b) = (or x-values (list 0 1))
	for x in (mb-utils::a-b a b :length resolution)
	do (format stream "~f~T~f~%" x (funcall function x))))

(defun write-array (stream y-values &key (x-values (0-n (length y-values) :type 'vector)))
  "Returns the filename containing y-values as the second #\TAB delimited column.
The first column currently only contains consecutive integers running from 0.
TODO: 
1. allow N dimensional array, where the Nth row contains data in the Nth dimension.
2. only write points where the first dimension is within x-values.
3. allow y-values, z-values etc, and modify task 2 accordingly"
  (loop for x across x-values
	for y across y-values
	do (format stream "~f~T~f~%" x y)))

(defun write-data-1 (stream target &rest plist)
  "TARGET should be either a function or an array"
  (typecase target
    (function (apply #'write-function stream target plist))
    (array (apply #'write-array stream target plist))
    (t (error "Unknown type ~a for target ~a" (type-of target) target))))
;;(write-data-1 t #'sqrt :resolution 10)

(defun write-data (target &rest plist)
  "Writes a data file based on TARGET."
  (with-temporary-file (stream)
    (if (consp target)
      (if (eql (first target) :d)
      ;; skip the initial :d
	(apply #'write-data-1 stream (append (rest target) plist))
	(error "Wrong data format: ~a" target))
      (apply #'write-data-1 stream target plist))))
;;(write-data `(:d ,#'sqrt) :resolution 10)

(defun line-1 (data &key (with :lines) title x-values)
  "Converts GP-LINES and PLIST to gnuplot string and writes it to
STREAM. It returnes the path to the DATAFILE."
  (awhen (write-data data :x-values x-values)
    (let ((title (case title
		   (:default nil)
		   ((:notitle nil) "notitle")
		   (t (format nil "title \"~a\"" title)))))
      (values (format nil "'~a' with lines~@[ ~a~]" (namestring it) title)
	      it))))
;;(line-1 `(:d ,#'sqrt :x-values (0 2)) :title "qwe" :x-values '(0.3 0.7))

(defun line (expression)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM.
Here graph and plot is the same"
  (if (and (consp expression) (eql (first expression) :l))
    (apply #'line-1 (rest expression))
    ;; else expression is just the data expression
    (line-1 expression)))
;;(line `(:l (:d ,#'sqrt :x-values (0 2)) :title "qwe" :x-values (0.3 0.7)))
;;(line `(:d ,#'sqrt :x-values (0 2)))

(defun graph (expression)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM.
Here graph and plot is the same"
  (concat
   (if (and (consp expression) (member (first expression) '(:g :p)))
     ;; extract and process properties 
     (loop for x on (rest expression)
	   until (keywordp (first x))
	   collect (line (first x))
	   ;; finally collect (process-properties x)
	  )
     ;; else expression is a simple line expression
     (list (line expression)))
   :pre "plot " :in ", "))
;;(graph `(:g (:d ,#'sqrt :x-values (0 2)) (:d ,#'sqrt :x-values (0 2))))
;;(graph `(:d ,#'sqrt :x-values (0 2)))
;;(graph #'sqrt)
;;(trace graph)

(defun graphs (expressions)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM."
  (concat expressions :in (newline) :key #'graph))
;;(graphs `(,#'sqrt))
;;(trace graphs)

(defun script (expressions)
  "Returns a gnuplot script for plotting unary FUNCTION from A to B
with N points."
  (with-temporary-file (s)
    (format s "set terminal pdf~%")
    (format s "set output '~a.pdf'~%" (namestring s))
    (format s "~a" (graphs expressions))))
;;(script `(,#'sqrt))
;;(trace script)

(defun plot (&rest expressions)
  "Returns a gnuplot script for plotting unary FUNCTION from A to B
with N points."
  (destructuring-bind (scriptpath datapath)
      (maptree #'namestring (multiple-value-list (script expressions)))
    (ext:execute "/usr/bin/gnuplot" (namestring scriptpath))
    (list :script scriptpath :data datapath :pdf (format nil "~a.pdf" scriptpath))))
;;(gp:plot `(:p ,#'sqrt ,#'sq (:l (:d ,#'(lambda (x) (sq (sin x))) :x-values (0.01 1)) :title "Geir")))
;;/ssh:ssh:/
