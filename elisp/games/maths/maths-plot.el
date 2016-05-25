"Ok, we must do some stuff here...
o create a temporary .dat file for the points
* create a temporary (?) script file
* invoke gnuplot from emacs with the script file as argument
* open the, say, pdf file
"

(defun gnuplot-ratings-user (user &optional (time-unit :hour))
  "RATINGS is a list of points. Each point is pair (DTTM RATING)"
  (maths-ratings-user user))

(cl-defun maths-data-temp-file (path-prefix ratings &optional (time-unit :hour))
  "RATINGS is a list of points. Each point is pair (DTTM RATING)"
  (let ((temp-gp-file (format "%s.dat" path-prefix)))
    (with-temp-file temp-gp-file
      (loop with now = (now)
	    for (rating iso-dttm) in (nreverse ratings)
	    do (insert (format "%s %s\n" (time- iso-dttm now :unit time-unit) rating))))
    temp-gp-file))

(defun maths-make-gnuplot-script (path-prefix)
  (let ((gp-script-path (format "%s.gp" path-prefix))
	(dat-path (format "%s.dat" path-prefix))
	(pdf-path (format "%s.pdf" path-prefix)))
    (with-temp-file gp-script-path
      (insert "set terminal postscript\n")
      (insert (format "set output '| ps2pdf - %s'\n" pdf-path))
      (insert (format "plot \"%s\" with lines\n" dat-path)))
    gp-script-path))

(defun gnuplot-plot-script (expression path-prefix)
  (let ((gp-script-path (format "%s.gp" path-prefix))
	(pdf-path (format "%s.pdf" path-prefix)))
    (with-temp-file gp-script-path
      (insert "set terminal postscript\n")
      (insert (format "set output '| ps2pdf - %s'\n" pdf-path))
      (insert (format "plot [0:2] %s\n" expression)))
    gp-script-path))

(cl-defun gnuplot (&key polynomial gnuplot-expression (prefix "gnuplot"))
  (let* ((path-prefix (make-temp-file prefix))
	 (expression (or (and polynomial (pfp polynomial :multiplication "*" :exponent "**"))
			 gnuplot-expression))
	 (gp-script-path (gnuplot-plot-script expression prefix)))
    (call-process "gnuplot" nil nil nil gp-script-path)
    (find-file (format "%s.pdf" prefix))))
;;(gnuplot)
;;(gnuplot :polynomial (reduce #'fp* (elliptic-fractions)))

(cl-defun gnuplot-ratings (ratings &optional (time-unit :hour) (prefix "gnuplot"))
  "RATINGS is a list of points. Each point is pair (DTTM RATING)"
  (let* ((path-prefix (make-temp-file prefix))
	 (dat-path (maths-data-temp-file path-prefix ratings time-unit))
	 (gp-script-path (maths-make-gnuplot-script path-prefix)))
    (call-process "gnuplot" nil nil nil gp-script-path)
    (find-file (format "%s.pdf" path-prefix))))
;;(gnuplot-ratings (maths-db-user-ratings "qwe"))
