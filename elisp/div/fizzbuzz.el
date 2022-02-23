(cl-defun fizz-buzz-sequence (&optional (n 20))
  "Return the first N words in a correct Fizz buzz game sequence."
  (loop for i from 1 to n collect
	(if (zerop (mod i 3))
	  (if (zerop (mod i 5))
	    "Fizz Buzz" i)
	  (if (zerop (mod i 5))
	    "Buzz" i))))
;;(fizz-buzz-sequence)

(cl-defun fizz-buzz (&optional (n 20))
  "Return a string with the first N words in a correct Fizz buzz
game sequence."
  (concat* (fizz-buzz-sequence n) :key #'sstring :in ", "))
;;(fizz-buzz 35)

(provide 'fizzbuzz)
