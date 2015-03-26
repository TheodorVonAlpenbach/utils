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

(defun write-2d-array (stream array)
  "Returns the filename containing y-values as the second #\TAB delimited column.
The first column currently only contains consecutive integers running from 0.
TODO: 
1. allow N dimensional array, where the Nth row contains data in the Nth dimension.
2. only write points where the first dimension is within x-values.
3. allow y-values, z-values etc, and modify task 2 accordingly"
  (loop for (x y) in (array->tree array)
	do (format stream "~f~T~f~%" x y)))

(defun write-array (stream y-values &key x-values x-range)
  "Returns the filename containing y-values as the second #\TAB delimited column.
The first column currently only contains consecutive integers running from 0.
TODO: 
1. allow N dimensional array, where the Nth row contains data in the Nth dimension.
2. only write points where the first dimension is within x-values.
3. allow y-values, z-values etc, and modify task 2 accordingly"
  (if (= (array-rank y-values) 2)
    (write-2d-array stream y-values)
    (loop for x across (or x-values (0-n (length y-values) :type 'vector))
	for y across y-values
	  do (format stream "~f~T~f~%" x y))))
;;(write-array t #2A((1 2)))

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
      (format out "'~a' with ~a~@[ ~a~]"
	(namestring it) (key->gp-name with) title))))
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

(defun write-property (key value out)
  (format out "set ~a ~a~%" (key->gp-name key) value))
;;(write-property :lmargin 3 t)

(defun write-margins (margins out)
  (loop for (key value) in (dispatch-margins margins)
	if value do (write-property key value out)))
;;(write-margins '((1 3) (2 4)) t)

(defun write-range (range out)
  (format out "[~a:~a]" (first range) (second range)))
;;(write-range '(2 3) t)
;;(write-property :xrange (write-range '(2 3) nil) t)

(defun graph-1 (out lines &key x-range y-range xlabel ylabel)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM. Here graph and plot is the same"
  (when lines
    (when x-range (write-property :xrange (write-range x-range nil) out))
    (when y-range (write-property :yrange (write-range y-range nil) out))
    (when xlabel (write-property :xlabel xlabel out))
    (when ylabel (write-property :ylabel ylabel out))
    (princ "plot " out)
    (format-list out (listify lines) #'(lambda (out x) (line out x))
		 :in ", ")))

(defun graph (out expression)
  "Converts GP-LINES and title to gnuplot string and writes it to
STREAM. Here graph and plot is the same"
  (when expression
    (if (graph-p expression)
      (apply #'graph-1 out (rest expression))
      (graph-1 out expression))))
;;(graph t `((:l (:d ,#'sqrt :x-values (0 2))) (:l (:d ,#'sqrt :x-values (0 2)))))
;;(graph t `(:d ,#'sqrt :x-values (0 2)))
;;(graph t `(,#'sqrt ,#'sqrt))
;;(untrace graph)

(defun key->gp-name (key)
  (string-downcase (symbol-name key)))

(defun terminal-type (terminal)
  (first (listify terminal)))

(defun terminal->pathtype (terminal)
  (let ((terminal-type (terminal-type terminal)))
    (case terminal-type
      ((:tex :latex :cairolatex) "tex")
      (t (key->gp-name terminal-type)))))
;;(mapcar #'terminal->pathtype '((:cairolatex) (:latex) :tex :pdf :bogus))

(defun write-terminal (terminal out)
  (format out "set terminal ~a" (key->gp-name (terminal-type terminal)))
  (if (consp terminal)
    (format out " roman ~a" (third terminal)))
  (terpri out))
;;(write-terminal '(:latex :fontsize 6) t)

(defun script (scriptpath expression terminal)
  "Returns a gnuplot script for plotting unary FUNCTION from A to B
with N points."
  (with-open-file (out scriptpath :direction :output :if-exists :supersede)
    (let* ((type (terminal->pathtype terminal))
	   (target (namestring (make-pathname :type type :defaults out))))
      (write-terminal terminal out)
      (format out "set output '~a'~%" target)
      (graph out expression)
      target)))
;;(script nil `(:g ,#'sqrt ,#'sqrt) :pdf)
;;(trace script)

(defun plot (expression &key directory name (terminal :pdf) margins)
  "Returns a gnuplot script for plotting unary FUNCTION from A to B
with N points."
  (let* ((scriptpath (gp-path directory name "gp"))
	(target (script scriptpath expression terminal)))
    (ext:execute "/usr/bin/gnuplot" scriptpath)
    (list :script scriptpath :target target)))
;;(gp:plot `(:p ,#'sqrt ,#'sq (:l (:d ,#'(lambda (x) (sq (sin x))) :x-range (0.01 1)) :title "Geir")))
;;(gp:plot `(:l (:d ,#'sqrt :resolution 10) :with :linespoints))
;;(gp:plot `((:l (:d ,(lambda (x) (sq x)) :x-values (0 1) :resolution 10) :with :linespoints) (:l (:d ,(lambda (x) (- 2 (sq x))) :x-values (1 2) :resolution 10) :with :linespoints)))

(defun version-number (name)
  (if (stringp name)
    (let ((pos (1+ (position-if-not #'digit-char-p name :from-end t))))
      (values (subseq name pos) (subseq name 0 pos)))
    (warn "argument is not a string")))
;;(version-number "qwe")

(defun new-name-version (name &optional length)
  "Implement LENGTH later"
  (multiple-value-bind (n string) (version-number name)
    (format nil "~a~a"
      (or string "tmp")
      (1+ (or (and n (parse-integer n :junk-allowed t)) 0)))))
;;(new-name-version "qwe")

(defun new-pathname (path &optional new-name)
  (make-pathname
   :name (or new-name (new-name-version (pathname-name path)))
   :defaults path))
;;(new-pathname "~/projects/utils/lisp/graphics/")

(defun gp-line-p (line target &optional (plot-string "plot"))
  (awhen (search target (string-trim '(#\Space #\Tab) line))
    (zerop it)))

(defun plot-line-p (line &optional (plot-string "plot"))
  (gp-line-p line plot-string))
;;(plot-line-p "  plot")

(defun output-line-p (line)
  (let ((tokens (split-by-char line #\Space t)))
    (and (= (length tokens) 3)
	 (destructuring-bind (set output path) tokens
	   (and (string= set "set")
		(string= output "output")
		tokens)))))
;;(output-line-p "set output '/tmp/PM-97-100.pdf'")

(defun gp-modify-output (lines new-path)
  "Ugly but works"
  (loop for l in lines
        for tokens = (output-line-p l)
	if tokens collect (format nil "set output '~a'"
			      (make-pathname :name (pathname-name new-path)
					     :defaults (string-trim "'" (third tokens))))
	else collect l))
;;(gp-modify-output (file->lines "/tmp/PM.gp") "/tmp/PM-97-100.gp")

(defun reset-range-in-script (scriptpath x-range &optional new-name)
  (let* ((lines (file->lines scriptpath))
	 (pos (position-if #'plot-line-p lines))
	 (new-path (new-pathname scriptpath new-name)))
    (with-open-file (out new-path :direction :output :if-exists :supersede)
      (setf lines (gp-modify-ouput lines new-path))
      (write-lines (subseq lines 0 pos) out)
      (write-property :xrange (write-range x-range nil) out)
      (write-lines (subseq lines pos) out)
      (pathname out))))
;;(reset-range-in-script "/tmp/PM.gp" '(97 100) "PM-97-100")

(defun rangify-name (path range)
  (format nil "~a-~a-~a" (pathname-name path) (first range) (second range)))
;;(rangify-name "/tmp/PM.gp" '(97 100))

(defun reset-range (scriptpath x-range &optional (new-name (rangify-name scriptpath x-range)))
  (let ((scriptpath (reset-range-in-script scriptpath x-range new-name)))
    (ext:execute "/usr/bin/gnuplot" (namestring scriptpath))
    (list :script (namestring scriptpath) :target (namestring (make-pathname :type "pdf" :defaults scriptpath)))))
;;(reset-range "/tmp/PM.gp" '(97 100))

(defun zoom-plot (scriptpath zoom-factor &optional (dimension :x))
  "Zooms first plot defined in scriptpath by ZOOM-FACTOR.
It only works with DIMENSION :X currently." )


;;; convert fns to arrays, replot
;;; write merge function with arrays
;;; write merge function with segment
(defvar testlength 100)
(defun testarray (fn a b)
  (tree->array (loop for x in (a-b a b :length testlength)
		     collect (list x (funcall fn x)))))
;;(testarray #'sq 0 1)
;;(testarray #'(lambda (x) (- 2 (sq x))) 1 2)

(defun mergearray (arr1 arr2)
  (let ((tree1 (array->tree arr1))
	(tree2 (array->tree arr2)))
    (let* ((y0 (second (first (last tree1))))
	   (y-1 (second (first (last tree1 2))))
	   (y1 (- (* 2 y0) y-1))
	   (z0 (second (first tree2)))
	   (z1 (second (second tree2)))
	   (a (/ (- y0 y1) (- z0 z1)))
	   (b (- y0 (* a z0)))
	   (as (a-b 0 testlength :length testlength :key #'(lambda (x) (+ (* (- a 1) (sq (/ (- x testlength) testlength))) 1))))
	   (bs (a-b b 0 :length testlength :direction :auto)))
      (values (tree->array (loop for (x y) in tree2
			 for a in as
			 for b in bs
				 collect (list x (+ (* a y) b))))
	      (list :y0 y0 :y1 y1 :z0 z0 :z1 z1 :a a :b b :as as :bs bs)))))
;;(mergearray (testarray #'sq 0 1) (testarray #'(lambda (x) (- 2 (sq x))) 1 2))

(defun merge-test ()
  (gp:plot `(:p ((:l (:d ,(testarray #'sq 0 1)) :with linespoints)
		 (:l (:d ,(testarray #'(lambda (x) (- 2 (sq x))) 1 2))  :with linespoints)
		 (:l (:d ,(mergearray (testarray #'sq 0 1) (testarray #'(lambda (x) (- 2 (sq x))) 1 2))) :with linespoints))
;;		:x-range (1.8 2)
		)))
;;(merge-test)
