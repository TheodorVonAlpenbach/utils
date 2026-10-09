(defun lehmer-encode (lehmer-code)
  "Convert lehmer-code to an integer"
  (scalar-product (nreverse (factorials (length lehmer-code))) lehmer-code))
;;(lehmer-encode '(2 2 1))

(defun factorials-including (n)
  "Return all factorials less or equal to N"
  (cl-loop for i from 1
	   for p = 1 then (* p i)
	   while (<= p n) collect p))
;;(factorials-including 24)

(defun lehmer-factorials (n length)
  (nreverse (if length (factorials length) (factorials-including n))))

(defun lehmer-decode (n &optional length)
  "Convert lehmer-code to an integer"
  (cl-loop for f in (lehmer-factorials n length)
	   for r = n then b
	   for (a b) = (cl-floor r f)
	   collect a))
;;(lehmer-decode 17)
;;(lehmer-encode (lehmer-decode 17))
;;(factorials 17 1 (1+ 17))

(defun lehmer-code (permutation)
  (cl-loop for (x . rest) on permutation
	   while rest collect (cl-count x rest :test #'>)))

(defun lehmer-permutation (lehmer-code)
  (cl-loop with indices = (0-n (1+ (length lehmer-code)))
	   for c in lehmer-code
	   collect (draw-nth c indices) into permutation
	   finally return (nconc permutation indices)))
;;(lehmer-permutation '(2 2 1))

(defun encode-permutation (permutation)
  (lehmer-encode (lehmer-code permutation)))
;;(encode-permutation '(2 3 1 0))

(cl-defun decode-permutation (n &optional length)
  (lehmer-permutation (lehmer-decode n length)))
;;(decode-permutation 17)
;;(decode-permutation 1)

(provide 'lehmer-code)
