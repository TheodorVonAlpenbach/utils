(cl-defun sum (sequence &rest cl-keys)
  (apply #'reduce #'+ sequence cl-keys))
;;(sum '(1 2 3) :start 1 :initial-value 123)

(defun type-of-super (sequence)
  (typecase sequence
    (cons 'list)
    (otherwise (type-of sequence))))

(defmacro random-elt (sequence)
  (with-gensyms (gsequence)
    `(let ((,gsequence ,sequence))
       (elt ,gsequence (random (length ,gsequence))))))
;;(random-elt (a-b -10 10))

(defun remove-nth (n sequence)
  (concatenate (type-of-super sequence)
    (subseq sequence 0 n)
    (subseq sequence (incf n))))
;;(progn (setq qwe "abc") (remove-nth 1 qwe))
;;(progn (setq qwe '(0 1 2)) (remove-nth 1 qwe))

(defun remove-nths (positions sequence)
  "Intuitive extension of DELETE-NTH. POSITIONS is a integer list."
  (let ((deletions 0))
    (dolist (p positions sequence)
      (setq sequence (remove-nth (- p deletions) sequence))
      (incf deletions))))
;;(remove-nths '(1 3 7) "mats bergstrøm")
;;(remove-nths '(1 3 7) '(a b c d e f g h))

(cl-defun length* (sequence &key start end)
  (- (or end (length sequence)) (or start 0)))
;;(length* '(1 2 3) :start nil :end nil)
;;(subseq (a-b 1 10) -3)

(cl-defmacro as-list ((list sequence) &body body)
  `(let ((,list (coerce ,sequence 'list)))
     (prog1 (progn ,@body)
       (setf ,sequence (coerce ,list (type-of ,sequence))))))
;;(as-list (x "abc") (setf x (rest x)))

(cl-defun copy-if (cl-pred cl-seq &key key count from-end)
  "Return a copy of SEQ containing exactly the items in SEQ not satisfying PREDICATE.
This is a non-destructive function; it makes a copy of SEQ if
necessary to avoid corrupting the original SEQ. See also
`cl-remove-if-not'.

Keywords supported:  :key :count :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (if (listp cl-seq)
    (loop with max-count = (or count most-positive-fixnum)
	  with current-count = 0
	  with seq = (if from-end (reverse cl-seq) cl-seq)
	  with values = (if key (mapcar key seq) seq)
	  for x in seq
	  for v in values
	  if (= current-count max-count) return res
	  if (funcall cl-pred v)
	    do (incf current-count) and
	    collect x into res
	  finally (return res))
    ;; else we convert cl-seq to a list, call ourselves, and convert back
    (coerce (copy-if cl-pred (coerce cl-seq 'list)
	      :key key :count count :from-end from-end)
	    (type-of cl-seq))))
;;(copy-if #'oddp '(1 2 3 4) :key #'1+ :count 1 :from-end nil)
;;(copy-if #'always "12345" :count 0)

(cl-defun copy (x cl-seq &key (test #'eql) key count from-end)
  (copy-if (bind test x 1) cl-seq :key key :count count :from-end from-end))
;;(copy 3 '(1 2 3 4) :key #'1+ :test #'<)

(defun elt-random (sequence)
  "Returns a random element in SEQUENCE"
  (elt sequence (random (length sequence))))

(cl-defmacro draw-nth (n sequence)
  "Draw the Nth element from sequence.
DRAWING is an operation where one or more elements are returned
from a sequence and where these elements are deleted from the
sequence as a side effect:
\n(let ((s '(a b c))) (list (draw 'c s) s)) ==> (c (a b))
\n(let ((s '(a b c))) (list (draw-nth 1 s) s)) ==> (b (a c))"
  `(prog1 (elt ,sequence ,n)
     (setf ,sequence (remove-nth ,n ,sequence))))
;;(let ((s (a-b 1 3))) (list (draw-nth 0 s) s))

(cl-defmacro draw-random (sequence)
  "Draw a random element in SEQUENCE.
See `draw-nth' for a definition of the draw operation."
  (with-gensyms (gi)
    `(let ((,gi (random (length ,sequence))))
      (draw-nth ,gi ,sequence))))
;;(let ((s (a-b 1 10))) (list (draw-random s) s))

(cl-defmacro draw (x cl-seq &rest cl-keys)
  "Draw all elements from SEQUENCE that matches X.
This implementation seems spurious. What if there is no
occurrences? Then it seems to delete all nil elements in
sequence, which is probably not the intention. Also, it should
clarify if only one element is drawn, or if all elements matching
X are drawn.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn ITEM SEQUENCE [KEYWORD VALUE]...)"
  `(draw-if (bind (or ,(popf cl-keys :test) #'eql) ,x 1) ,cl-seq ,@cl-keys))
;;(let ((s "qweqwe")) (list (draw ?e s :key #'identity :start 4) s))

(cl-defmacro draw-if (predicate sequence &rest cl-keys)
  "Draws all elements matching PREDICATE from SEQUENCE.
A DRAW operation is a sequence modifying operation. It deletes
the matching elements from the sequence, and returns those
elements.
\nKeywords supported:  :test :key :count :start :end :from-end
\n(fn SEQUENCE PREDICATE [KEYWORD VALUE]...)"
  `(prog1 (copy-if ,predicate
	    (subseq ,sequence (or ,(getf cl-keys :start) 0) ,(getf cl-keys :end))
	    ,@(mremf cl-keys :start :end))
     (setf ,sequence (cl-delete-if ,predicate ,sequence ,@cl-keys))))
;;(let ((s '(0 1 2 3 0 0))) (list (draw-if #'evenp s :key #'identity :count 2) s))
;;(let ((s "qweqwe")) (list (draw-if (bind #'eql ?e) s :key #'identity :end 4) s))

(cl-defun positions (item sequence &key (key #'identity) (test #'eq))
  (loop for x in (coerce sequence 'list)
	for i from 0
	if (funcall test item (funcall key x))
	collect i))
;;(positions 'a '(a b c a))

;;; min/max function
;;; This is a mess.
;;; MINIMUMS is ok. It must also be implemented by sort.
;;; MIN-ELT is also useful, and searches linearly through the sequence to find min.
;;; However, the name wrong, since it returns the values (min-elt min-position min-value). 
;;; MINIMUMS is absolutely not ok, since it is supposed to return the min-elt of MIN-ELT.

;;; The solution to all this is to rename MIN-ELT to MINIMUM, and then DEFSUBSTing
;;; min-element, min-position, and min-value to the return values of MINIMUM.

;;; TODO: need to do a massive gfind to modify other utils due to
;;; these changes. Best to log off here first, and then make sure
;;; Drive updates and then do this at home

(defun nminimums (cl-seq cl-pred &rest cl-keys)
  "Find all the minimum objects in SEQ according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (let* ((key-fn (second (member :key cl-keys)))
	 (sorted-seq (apply #'sort* cl-seq cl-pred cl-keys))
	 (first-min (if key-fn (funcall key-fn (first-elt sorted-seq)) (first-elt sorted-seq))))
    (loop for elt in sorted-seq
	  while (not (funcall cl-pred first-min (if key-fn (funcall key-fn elt) elt)))
	  collect elt)))
;;(nminimums '(1 2 3 0 4 0 5) #'< :key #'1+)

(defun minimums (cl-seq cl-pred &rest cl-keys)
  "Find all the minimum objects in SEQ according to PREDICATE.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (apply #'nminimums (copy-sequence cl-seq) cl-pred cl-keys))
;;(minimums '(1 2 3 0 4 0 5) #'<)

(cl-defun nminimum-nokey (seq test)
  "Helper for `nminimum'. Same as nminimum, but without key and from-end.
This function is not intended for use."
  (loop with min = (elt seq 0)
	with pos = 0
	for i from 1
	for x across seq
	when (funcall test x min)
	do (setf min x pos i)
	finally (return (values min pos))))
;;(nminimum-nokey (vector 1 2 3 0) #'<)

(cl-defun minimum (cl-seq &key (test #'<) key (start 0) end from-end)
  "Find the minimum of SEQ.
The returned object is the value pair (ELEMENT POSITION VALUE)
where ELEMENT is the minimum element in SEQ and POSITION is the
positition of that element in SEQ. VALUE is an element identical
to (funcall KEY ELEMENT). The default values of the supported
keywords TEST, KEY, AND FROM-END are #'<, #'IDENTITY, and NIL,
respectively.

\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (if from-end
    (minimum (nreverse cl-seq) :test test :key key)
    (destructuring-bind (min pos)
	(nminimum-nokey
	 (if key
	   (map 'vector key (subseq cl-seq start end))
	   (coerce (subseq cl-seq start end) 'vector))
	 test)
      (values (elt cl-seq pos) (+ pos start) min))))
;;(let ((db '((0 2) (1 -1) (1 2)))) (list (nminimum db :key #'second) db))

(cl-defun min-position (&rest args)
  "Returns position of the minimum element in SEQUENCE.
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (second (apply #'minimum args)))
;;(min-position '(1 2 1 3) :key #'1+ :from-end t)

(cl-defun min-element (&rest args)
  "Returns minimum element in SEQUENCE
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (first (apply #'minimum args)))
;;(min-element '(1 2 1 3) :key #'1+)

(cl-defun list< (list1 list2 &key (test #'<) (key #'identity))
  "Lexical list comparison"
  (loop with tests = (if (atom test)
		       (make-list (min (length list1) (length list2)) test)
		       test)
	for x1 in list1
	for x2 in list2
	for test in tests
	for y1 = (funcall key x1)
	for y2 = (funcall key x2)
	if (funcall test y1 y2) return t
	if (funcall test y2 y1) return nil
	;; if we arrive here, return min #'length
	finally (return (< (length list1) (length list2)))))
;;(list< '((a a) (a)  (b b)  (a) (b)) '((b b) (a) (b) (a a) (a)) :key #'length)
;;(list< '(1 "b") '(1 "b") :test #'string< :key #'sstring)
;;(list< '("n" "c1" "c3") '("n" "c1" "c2") :test #'string<)

(cl-defun list> (list1 list2 &key (test #'<) (key #'identity))
  (list< list2 list1 :test test :key key))
;;(list> '(1 2 3 5) '(1 2 3 3 3))

(cl-defun subseq* (seq &optional (start 0) (end nil))
  "Same as SUBSEQ but takes negative limit arguments (meaning from end).
Note that this is exactly the same code as in `substring*' !
Note! This function is now obsolete. Use `subseq' instead."
  (warn "SUBSEQ* function is now obsolete, since subseq now has the same behaviour.")
  (unless (null seq)
    (let ((n (length seq)))
    (subseq seq 
      (mod start n)
      (when (numberp end) (mod end n))))))
;;(subseq* '(a b c d) 1 -1)
;;(subseq '(a b c d) 1 -1)
;;(subseq* "abcde" 1 -1)
;;(subseq* nil 1 -1)

;;; Here is the sequence version (not so fast)
(defun deltas (sequence)
  "Returns the canonical deltas around numbers in sequence.
If A, B, C are consecutive numbers, the delta for B is (- (/ (+ A C) 2) B)."
  (coerce (deltas-list (coerce sequence 'list)) (type-of sequence)))
;;(deltas (vector 0 5 10 14 17 20 22))

(provide 'mb-sequences)
