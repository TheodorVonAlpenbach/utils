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

(defvar *gp-unique-id* 0)
(defun gp-unique-id (&optional reset)
  (when reset
    (warn "Resetting unique ID")
    (setf *gp-unique-id* 0))
  (prog1 (format nil "~4,'0d" *gp-unique-id*)
    (incf *gp-unique-id*)))
;;(gp-unique-id)

(defun gp-unique-name (&key (prefix "gp") type reset)
  (format nil "~a~a~@[.~a~]" prefix (gp-unique-id reset) type))
;;(gp-unique-name :prefix "gp" :type "pdf")

(defun gp-directory (directory &optional (tmp-dir "/tmp/"))
  (if directory
    (if (char= (last-elt directory) #\/)
      directory
      (concatenate 'string directory "/"))
    tmp-dir))
;;(mapcar #'gp-directory '(nil "qwe" "qwe/"))

(defun gp-filename (name type)
  (if name
    (format nil "~a~@[.~a~]" name type)
    (gp-unique-name :type type)))
;;(gp-filename "mats" "pdf")

(defun gp-path (directory name type)
  (concatenate 'string (gp-directory directory) (gp-filename name type)))
;;(gp-path "dir" "mats" "pdf")

(defun write-function (stream function &key x-values (resolution 100) x-range)
  "Returns the filename containing RESOLUTION samples of FUNCTION in the interval X-VALUES.
TODO: Handle n-ary functions and generalize X-VALUES to N dimensions."
  (loop with (a b) = (or x-values x-range (list 0 1))
	for x in (mb-utils::a-b a b :length resolution)
	do (format stream "~f~T~f~%" x (funcall function x))))
;;(trace write-function)

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
;;(trace write-data-1)

(defun write-data (stream target &rest plist)
  "Writes a data file based on TARGET."
  (with-temporary-file (stream)
    (if (atom target)
      (apply #'write-data-1 stream target plist)
      (apply #'write-data-1 stream (append (rest target) plist)))))
;;(write-data t `(:d ,#'sqrt :x-values (.5 1.0)) :x-range nil)
;;(write-data t #'sqrt)
;;(trace write-data)

(defun line-1 (out data &key (with :lines) title x-range y-range)
  "Converts GP-LINES and PLIST to gnuplot string and writes it to
STREAM. It returnes the path to the DATAFILE."
  (awhen (write-data out data :x-range x-range)
    (let ((title (case title
		   (:default nil)
		   ((:notitle nil) "notitle")
		   (t (format nil "title \"~a\"" title)))))
      (format out "'~a' with lines~@[ ~a~]" (namestring it) title))))
;;(line-1 t `(:d ,#'sqrt :x-values (.5 1.0)) :title "qwe" :x-range nil)
;;(untrace line-1)

(defun line-p (x &optional with-subline-p)
  (and (consp x)
       (member (first x) (if with-subline-p '(:l :d) '(:l)))))
;;(mapcar #'line-p '(nil #'sin (:l bla) (:d bla)))
;;(mapcar (bind #'line-p t) '(nil #'sin (:l bla) (:d bla)))

(defun line (out expression)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM.
Here graph and plot is the same"
  (if (line-p expression)
    (apply #'line-1 out (rest expression))
    ;; else expression is just the data expression
    (line-1 out expression)))
;;(line t `(:l (:d ,#'sqrt :x-values (0 2)) :title "qwe" :x-range (0.3 0.7)))
;;(line t `(:d ,#'sqrt :x-values (0 2)))
;;(line t #'sqrt)
;;(trace line)

(defun graph-p (expression)
  (and (consp expression)
       (member (first expression) '(:g :p))))

(defun dispatch-margins (margins)
  (destructuring-bind (l-r &optional t-b) (listify margins)
    (destructuring-bind (ml &optional mr) (listify l-r)
      (destructuring-bind (mt &optional mb) (listify t-b)
	(cut (list :lmargin ml :rmargin mr :tmargin mt :bmargin mb))))))
;;(dispatch-margins 1)

(defun format-property (out property-key value)
  (format out "set ~a ~a~%" (key->gp-name property-key) value))
;;(format-property t :lmargin 3)

(defun format-margins (out margins)
  (loop for (key value) in (dispatch-margins margins)
	if value do (format-property out key value)))
;;(format-margins t '((1 3) (2 4)))

(defun graph (out expression &key x-range y-range)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM. Here graph and plot is the same"
  (when expression
    (princ "plot " out)
    (let* ((target (if (graph-p expression)
		     (second expression)
		     expression))
	   (targets (if (or (atom target) (line-p target t))
		      (list target) target)))
      (format-list out targets #'(lambda (out x) (line out x))
		   :in ", "))))
;;(graph t `((:l (:d ,#'sqrt :x-values (0 2))) (:l (:d ,#'sqrt :x-values (0 2)))))
;;(graph t `(:d ,#'sqrt :x-values (0 2)))
;;(graph t `(,#'sqrt ,#'sqrt))
;;(untrace graph)

(defun script (scriptpath expression terminal)
  "Returns a gnuplot script for plotting unary FUNCTION from A to B
with N points."
  (with-open-file (out scriptpath :direction :output :if-exists :supersede)
    (let* ((type (terminal->type terminal))
	   (target (namestring (make-pathname :type type :defaults out))))
      (format out "set terminal ~a~%" type)
      (format out "set output '~a'~%" target)
      (graph out expression)
      target)))
;;(script nil `(:g ,#'sqrt ,#'sqrt) :pdf)
;;(trace script)

(defun key->gp-name (key)
  (string-downcase (symbol-name key)))

(defun terminal->type (terminal)
  (key->gp-name (first (listify terminal))))
;;(terminal->type :pdf)

(defun plot (expression &key directory name (terminal :pdf) margins)
  "Returns a gnuplot script for plotting unary FUNCTION from A to B
with N points."
  (let* ((scriptpath (gp-path directory name "gp"))
	(target (script scriptpath expression terminal)))
    (ext:execute "/usr/bin/gnuplot" scriptpath)
    (list :script scriptpath :target target)))
;;(gp:plot `(:p ,#'sqrt ,#'sq (:l (:d ,#'(lambda (x) (sq (sin x))) :x-range (0.01 1)) :title "Geir")))
;;(gp:plot `(:d ,#'sqrt))
;;/ssh:ssh:/
;;(ext:execute "/usr/bin/gnuplot" "/tmp/gp0017.gp")
