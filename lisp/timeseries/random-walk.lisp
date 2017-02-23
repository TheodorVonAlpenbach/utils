(in-package :timeseries)

(defun generate-random-walk (n amplitude &optional (start 0))
  (loop for i below n
	for p2p = (* 2 amplitude)
	for y = start then (+ y (- amplitude (random p2p))) 
	collect y))
;;(generate-random-walk 100 2.0)
;;(generate-random-walk 3 2)
;;(gp::plot `(generate-random-walk 20 2.0))
;;(trace generate-random-walk)

(defun timestamps (n &optional delta-t start-t)
  "Return a list of length N with Ith element (+ START-T (* I DELTA-T))"
  (let ((delta-t (or delta-t 1)))
    (loop for i below n
	  for elt = (or start-t 0) then (+ elt delta-t)
	  collect elt)))
;;(timestamps 5)
;;(untrace timestamps)

(defun write-random-walks (n amplitudes filename
			   &optional starts (delta-t 1) (start-t))
  "Generate M timeseries of length N and write to FILENAME.
M is the length of AMPLITUDES."
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'float (lambda (s f) (format s "~,2f" f)))
    (write-csv-file
     (transpose-tree
      (cons (timestamps n delta-t start-t)
	    (loop for amplitude in amplitudes
		  for start in (or starts
				   (make-list (length amplitudes)
					      :initial-element 0))
		  collect (generate-random-walk n amplitude start))))
     filename
     :column-separator #\;)))
;;(write-random-walks 3 '(1.0 2.0) "/home/mbe/projects/chess/TPTFilter/test/random-walk.csv" '(.5 .3))
;;(write-random-walks 10 '(1.0 2.0) "~/projects/chess/TPTFilter/test/random-walk2.csv" nil .01 (coerce (get-unix-time) 'double-float))
