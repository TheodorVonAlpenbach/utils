(defun char-to-integer (char)
  (let ((n (- (char-int char) 48)))
    (assert (<= 0 n 9))
    n))
;;(map 'list #'char-to-integer "0123456789")

(defun number-to-digits (n &optional (sequence-type 'list))
  (map sequence-type #'char-to-integer (write-to-string n)))
;;(number-to-digits 1234567890 'vector)

(defun digit-sum (n)
  (reduce #'+ (number-to-digits n)))
;;(digit-sum 12345)

(defun faculty (n)
  (loop for i from 2 to n
	for f = i then (* i f)
	finally (return f)))
;;(faculty 7)
;;(* 2 2 2 3 3 5 7)

(defun ! (n) (faculty n))

(defun binomial-coefficient (n k)
  (/ (! n) (! n) (! (- n k))))

(defun sq (x) (* x x))

(defun li (x) (/ x (log x)))

(defun mb-split-sequence (sequence separator &optional ignore-empty-p)
  "Slow version of cl-utils' SPLIT-SEQUENCE.
It does not include the SEPARATORs"
  (let* ((a 0)
	(res (append
	      (loop for b = (position separator sequence :start a)
		    while b
		    collect (subseq sequence a b) into res
		    do (setf a (1+ b))
		    finally (return (append res (list (subseq sequence a))))))))
    (if ignore-empty-p
      (remove 0 res :key #'length) res)))
;;(mb-split-sequence '(5 2 3 4 5) 5 t)

(defun lines (string &optional (ignore-empty-p t))
  (mb-split-sequence string #\Newline ignore-empty-p))

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments. Performance depends on efficiency of general ADJUST-ARRAY in the
host lisp -- for most cases a special purpose copying function is likely to
perform better."
  (let ((dims (array-dimensions array)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy.
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))

(defun popf (plist key)
  "Returns getf and operates a remf."
  (prog1 (getf plist key)
    (remf plist key)))
;;(let ((plist '(:a 1 :b 2))) (list (popf plist :c) plist))

;; Transfer these to mb-utils
(defun maximum (seq &key (test #'>) key (start 0) end from-end)
  (let* ((max (reduce #'max seq :key key :from-end from-end
		      :start start :end end))
	 (pos (position max seq :key key :from-end from-end
			:start start :end end)))
    (values max pos (elt seq pos))))


(defun minimum (seq &key (test #'>) key (start 0) end from-end)
  (let* ((min (reduce #'min seq :key key :from-end from-end
		      :start start :end end))
	 (pos (position min seq :key key :from-end from-end
			:start start :end end)))
    (values min pos (elt seq pos))))

;;(maximum '(5 5 3 1 1 4) :start 2)
;;(maximum '(0 0 0 1 1 4))

(defun skip-lines (stream n)
  "Move to util file"
  (loop repeat n while (read-line stream nil nil)))

(defun read-lines (stream &key start end remove-empty-p)
  "Move to util file"
  (when start (skip-lines stream start))
  (loop for line = (read-line stream nil nil)
     for i from (or start 0)
     while (and line (or (not end) (< i end)))
     if (not (and remove-empty-p (string= line "")))
     collect line))

(defun file->lines (filespec &rest args)
  (with-open-file (in filespec) (apply #'read-lines in args)))

(provide 'cl-utils)
