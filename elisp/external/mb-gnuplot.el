"Ok, we must do some stuff here...
o create a temporary .dat file for the points
* create a temporary (?) script file
* invoke gnuplot from emacs with the script file as argument
"

(defun gp-path (prefix &optional type)
  (concat prefix (case type (:pdf ".pdf") (:script ".gp") (:data ".dat") (t ""))))
;;(gp-path "qwe" :pdf)

(cl-defun gp-make-script-file (script prefix)
  "Creates a gnuplot script file containing SCRIPT and returns its file path."
  (with-temp-file (gp-path prefix :script)
      (insert "set terminal postscript\n")
      (insert (format "set output '| ps2pdf - %s'\n" (gp-path prefix :pdf)))
      (insert script)))

(defun gp-format-range (range)
  (if range
    (format "[%s:%s]" (first range) (second range))
    ""))

(cl-defun gp-make-data-file (points path)
  "Creates a data file for gnuplot"
  (let ((points (if (listp (first points)) 
		  points
		  (loop for i from 0
			for x in points
			collect (list i x)))))
    (with-temp-file path
      (loop for (x y) in points do (insert (format "%s %s\n" x y))))))

(cl-defun gp-data-script (points prefix &key x-range y-range)
  "Creates a gnuplot script file and returns its file path.
TODO: for xrange and yrange, use command
set xrange [1:2]
set yrange [2:3]
instead"
  (let ((data-path (gp-path prefix :data)))
    (gp-make-data-file points data-path)
    (gp-make-script-file (concat* (list (gp-format-range x-range)
					(gp-format-range y-range)
					(format "\"%s\" with lines" data-path))
			   :pre "plot " :in " " :discard-nil t)
			 prefix)))
;;(mb-gnuplot (loop for i below 100 collect (list i (sq i))) :type :points :x-range '(0 10) :y-range '(10 20))

(cl-defun gp-function-script (function prefix &key (x-range '(-1 1)) y-range (resolution 200))
  "Creates a gnuplot script file and returns its file path."
  (destructuring-bind (xmin xmax) x-range
    (gp-data-script
     (loop for x from xmin to xmax by (/ (- xmax xmin) (float resolution))
	   collect (list x (funcall function x)))
     prefix :x-range x-range :y-range y-range)))
;;(mb-gnuplot #'sqrt :x-range '(0 1))

(cl-defun gp-expression-script (expression prefix &key x-range y-range)
  "Creates a gnuplot script file and returns its file path."
  (gp-make-script-file (concat* (list (gp-format-range x-range)
				      (gp-format-range y-range)
				      expression)
			 :pre "plot " :in " " :discard-nil t)
		       prefix))

(require 'mb-polynomials)
(defun gp-pfp (fractional-polynomial)
  "Formats FRACTIONAL-POLYNOMIAL in gnuplot style"
  (pfp fractional-polynomial :multiplication "*" :exponent "**"))
;;(gp-pfp '(1 2 3.123))

(cl-defun gp-polynomial-script (expression prefix &rest args)
  "Creates a gnuplot script file and returns its file path."
  (apply #'gp-function-script (gp-pfp expression) prefix args))

(defun call-gnuplot (path)
  (message "Waiting for gnuplot...")
  (call-process* "gnuplot" path)
  (message "gnuplot finished!")
  (find-file (gp-path path-prefix :pdf))  )

(defun gp-float (x)
  (cl-float-limits)
  (cond
    ((= cl-most-positive-float x) "∞")
    ((= cl-most-negative-float x) "-∞")
    (t (format "%f" x))))
;;(mapcar #'gp-float (list cl-most-positive-float cl-most-negative-float float-pi 0))

(cl-defun mb-gnuplot (expression &key (type :function) (directory) (prefix "gnuplot") suffix x-range y-range title)
  "Creates a gnuplot script file, compiles it and shows the resulting pdf in a buffer.
The possible TYPEs are :function (the default), :points, :polynomial.
For DIRECTORY and SUFFIX see `make-temp-file'"
  (let* ((path-prefix (make-temp-file prefix nil suffix)))
    (case type
      (:function (gp-function-script expression path-prefix :x-range x-range :y-range y-range))
      (:expression (gp-expression-script expression path-prefix :x-range x-range :y-range y-range))
      (:polynomial (gp-polynomial-script expression path-prefix :x-range x-range :y-range y-range))
      (:points (gp-data-script expression path-prefix :x-range x-range :y-range y-range)))
    (call-gnuplot (gp-path path-prefix :script))))

(cl-defun mb-gnuplot (expression &key (type :function) (directory) (prefix "gnuplot") suffix x-range y-range title)
  "Creates a gnuplot script file, compiles it and shows the resulting pdf in a buffer.
The possible TYPEs are :function (the default), :points, :polynomial.
For DIRECTORY and SUFFIX see `make-temp-file'"
  (let* ((path-prefix (make-temp-file prefix nil suffix)))
    (case type
      (:function (gp-function-script expression path-prefix :x-range x-range :y-range y-range))
      (:expression (gp-expression-script expression path-prefix :x-range x-range :y-range y-range))
      (:polynomial (gp-polynomial-script expression path-prefix :x-range x-range :y-range y-range))
      (:points (gp-data-script expression path-prefix :x-range x-range :y-range y-range)))
    (call-gnuplot (gp-path path-prefix :script))))

;;(mb-gnuplot (loop for i below 100 collect (list i (sq i))) :type :points :x-range '(0 1))
;;(mb-gnuplot (loop for i below 100 collect (sq i)) :type :points)

(provide 'mb-gnuplot)
