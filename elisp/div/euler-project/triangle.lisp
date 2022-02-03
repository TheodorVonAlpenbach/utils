(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun parse-triangle-lines (triangle-lines)
  (coerce (loop for l in triangle-lines collect
		(coerce (loop for s in (mb-split-sequence
					(string-trim '(#\Space #\Tab) l) #\  t)
			      collect (parse-integer s))
			'vector))
	  'vector))

(defun parse-triangle-string (triangle-string)
  (parse-triangle-lines (lines triangle-string)))
;;(length (parse-triangle-string 018-problem-triangle))

(defun copy-triangle (triangle)
  (map 'vector #'copy-seq triangle))
;;(copy-triangle (parse-triangle-string 018-problem-triangle))

(defun triangle-max-path-indices (triangle)
  (cons 0
	(loop for i below (1- (length triangle))
	      for j = 0 then (if (< (svref (svref triangle (1+ i)) j)
				    (svref (svref triangle (1+ i)) (1+ j)))
			       (1+ j) j)
	      collect j)))

(defun maximum-path-sum (value-triangle)
  (let* ((acc-triangle (copy-triangle value-triangle)))
    (loop for i downfrom (- (length acc-triangle) 2) downto 0 do
	  (loop for j to i do
		(incf (svref (svref acc-triangle i) j)
		      (max (svref (svref acc-triangle (1+ i)) j)
			   (svref (svref acc-triangle (1+ i)) (1+ j))))))
    (let ((solution-path (loop for ij in (triangle-max-path-indices acc-triangle)
			       for triangle-level-i across value-triangle
			       collect (svref triangle-level-i ij))))
      (values (reduce #'+ solution-path) solution-path))))
