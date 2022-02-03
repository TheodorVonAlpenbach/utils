(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defparameter 018-sample-triangle
  "
				  3
				 7 4
				2 4 6
			       8 5 9 3"
)

(defparameter 018-problem-triangle
  "
				  75
				95 64
			       17 47 82
			     18 35 87 10
			    20 04 82 47 65
			  19 01 23 75 03 34
			 88 02 77 73 07 63 67
		       99 65 04 28 06 16 70 92
		      41 41 26 56 83 40 80 70 33
		    41 48 72 33 47 32 37 16 94 29
		   53 71 44 65 25 43 91 52 97 51 14
		 70 11 33 28 77 73 17 78 39 68 17 57
		91 71 52 38 17 14 91 43 58 50 27 29 48
	      63 66 04 68 89 53 67 30 73 16 69 87 40 31
	    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
)

(defun parse-triangle (triangle)
  (coerce (loop for l in (lines triangle) collect
		(coerce (loop for s in (mb-split-sequence
					(string-trim '(#\Space #\Tab) l) #\  t)
			      collect (parse-integer s))
			'vector))
	  'vector))
;;(length (parse-triangle 018-problem-triangle))

(defun copy-triangle (triangle)
  (map 'vector #'copy-seq triangle))
;;(copy-triangle (parse-triangle 018-problem-triangle))

(defun triangle-max-path-indices (triangle)
  (cons 0
	(loop for i below (1- (length triangle))
	      for j = 0 then (if (< (svref (svref triangle (1+ i)) j)
				    (svref (svref triangle (1+ i)) (1+ j)))
			       (1+ j) j)
	      collect j)))

(defun 018-solution (&optional (triangle 018-problem-triangle))
  (let* ((value-triangle (parse-triangle triangle))
	 (acc-triangle (copy-triangle value-triangle)))
    (loop for i downfrom (- (length acc-triangle) 2) downto 0 do
	  (loop for j to i do
		(incf (svref (svref acc-triangle i) j)
		      (max (svref (svref acc-triangle (1+ i)) j)
			   (svref (svref acc-triangle (1+ i)) (1+ j))))))
    (let ((solution-path (loop for ij in (triangle-max-path-indices acc-triangle)
			       for triangle-level-i across value-triangle
			       collect (svref triangle-level-i ij))))
      (values (reduce #'+ solution-path) solution-path))))
;;(018-solution)
;; => 1070
;; (75 95 47 87 82 75 73 28 83 32 91 78 58 73 93)
