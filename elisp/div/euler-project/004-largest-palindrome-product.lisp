(defun palindrome-p (sequence)
  (equal sequence (reverse sequence)))
;;(palindrome-p (write-to-string (* 999 999)))

(defun 004-solution ()
  "Find the largest palindrome made from the product of two 3-digit numbers."
  (first
   (sort (loop for i from 999 downto 100
	       append (loop for j from i downto 100
			    for ij = (* i j)
			    if (palindrome-p (write-to-string ij))
			    collect (list ij i j)))
     #'> :key #'first))  )
;;(time (004-solution))
;; => (906609 993 913)

(defun 004-solution-faster ()
  "Find the largest palindrome made from the product of two 3-digit numbers."
  (let ((max '(1 1 1)))
    (loop for i from 999 downto 100
	  while (> (* i i) (first max))
	  append (loop for j from i downto 100
		       for ij = (* i j)
		       while (> ij (first max))
		       if (palindrome-p (write-to-string ij))
		       do (setf max (list ij i j))))
    max))
;;(time (004-solution-faster))
;; => (906609 993 913)

