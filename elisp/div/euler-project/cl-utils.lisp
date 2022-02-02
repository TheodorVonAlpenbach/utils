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

(defun nminimum-nokey (vec test)
  "Helper for `nminimum'. Same as nminimum, but without key and from-end.
Note that the function fails, if VEC is empty. It is the onus of
the caller to avoid this.

This function is not intended for use outside of this module."
  (loop with min = (elt vec 0)
	with pos = 0
	for i from 0
	for x across vec
	when (funcall test x min)
	do (setf min x pos i)
	finally (return (values min pos))))
;;(nminimum-nokey (vector 1 2 3 0) #'<)

(defun minimum (seq &key (test #'<) key (start 0) end from-end)
  "Find the minimum of SEQ.
The returned object is the value triple (ELEMENT POSITION VALUE) where
ELEMENT is the minimum element in SEQ and POSITION is the positition
of that element in SEQ. VALUE is an element identical to (funcall KEY
ELEMENT). The default values of the supported keywords TEST, KEY,
START, END, and FROM-END are #'<, #'IDENTITY, 0 NIL, and NIL,
respectively."
  (when (plusp (length seq))
    (if from-end
      (minimum (nreverse seq) :test test :key key)
      (multiple-value-bind (min pos)
	  (nminimum-nokey
	   (if key
	     (map 'vector key (subseq seq start end))
	     (coerce (subseq seq start end) 'vector))
	   test)
	(let ((pos* (+ pos start)))
	  (values (elt seq pos*) pos* min))))))
;;(minimum (vector 1 2 3 0))

(defun maximum (list &rest args)
  "Find the maximum of SEQ. See MINIMUM for details."  
  (apply #'minimum list :test (let ((test (popf args :test)))
				(if test
				  (complement test)
				  #'>))
	 args))
;;(maximum '(5 5 3 1 1 4) :start 2)
;;(maximum '(0 0 0 1 1 4))
(provide 'cl-utils)
