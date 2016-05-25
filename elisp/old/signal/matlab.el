(defun filter-distance (filter1 filter2)
  (min-element (flatten (mat-subtract filter1 filter2)) :test #'> :key #'abs))
;;(filter-distance '((1 2) (1 2)) '((1 2) (1 2.1)))

(defun elliptic-T (matlab-filter &rest args)
  (cl-flet* ((ef (T) (apply #'elliptic-filter :T T args))
	     (d (T) (filter-distance (ef T) mf)))
    (let* ((mf matlab-filter)
	   (lower-T-d (loop for T = 1.0 then (/ T 2.0) 
				 for d = (d T)
				 when (minusp d) return (list T d)))
	   (upper-T-d (loop for T = (first lower-T-d) then (* 2.0 T) 
				 for d = (d T)
				 when (plusp d) return (list T d))))
      (loop for (T d) = (mapcar* #'average (transpose (list lower-T-d upper-T-d)))
	    for count below 100
	    do (setf (if (minusp d) lower-T-d upper-T-d) (list T (d T)))
	    when (< (abs d) 1E-4) return (list T d count (mat-subtract mf (ef T)))
	    finally return (list T d count (mat-subtract mf (ef T)))))))
;;(elliptic-T mf)
;;(filter-distance (elliptic-filter :T 20.98) mf)
;;mf((0.3721 2.1789 5.369 7.1243 5.369 2.1789 0.3721) (1.0 4.8822 10.3486 12.2021 8.4898 3.3311 0.5829))
;;(getf (elliptic-parameters) :As)51.6656510964487

(defun matlab-filter (filter)
  "Converts filter to matlab statements that defines the filter
coefficients in the canonic variables a b. The matlab code is
automatically saved to clipboard"
  (kill-new
   (concat
    (concat* (second filter)
      :pre "a = [" :in " " :suf "];\n" :key #'number-to-string)
    (concat* (first filter)
      :pre "b = [" :in " " :suf "];" :key #'number-to-string))))
;;(matlab-filter (elliptic-filter))

(defun parse-matlab-coefficients ()
  (let* ((s (current-kill 0))
	(parts (string-match*
		"\\([ab]\\) =[^[:digit:]]*\\(.*\\)[^>]*>>[^=][^[:digit:]]*\\(.*\\)"
		s :num '(1 2 3))))
    (maptree #'string-to-number 
	     (mapcar #'split-string
		     (if (string= (first parts) "a")
		       (nreverse (rest parts)) (rest parts))))))
;;(setq mf (parse-matlab-coefficients))

