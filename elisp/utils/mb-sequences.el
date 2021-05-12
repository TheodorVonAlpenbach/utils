(defun type-of-super (sequence)
  (typecase sequence
    (cons 'list)
    (otherwise (type-of sequence))))

(defmacro random-elt (sequence)
  (with-gensyms (gsequence)
    `(let ((,gsequence ,sequence))
       (message "%S" (length ,gsequence))
       (elt ,gsequence (random (length ,gsequence))))))
;;(random-elt '((a e) (b) (c) (d)))
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

(defun sequence-type (sequence)
  (cond ((listp sequence) 'list)
	((vectorp sequence) 'vector)
	((stringp sequence) 'string)))
;;(sequence-type '(1 2))

(cl-defmacro as-list ((list sequence) &body body)
  "Evaluate body with SEQUENCE treated as a list.
SEQUENCE is coerced to a list bound to the symbol LIST. The
evaluation of the last form in BODY is expected to be a list,
which is finally coerced to the same type as SEQUENCE."
  `(let ((,list (coerce ,sequence 'list)))
     (coerce (progn ,@body) (sequence-type ,sequence))))
(cl-indent 'as-list 1)

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
  "Return a copy of SEQ containing exactly the items matching X.
An element Y matches X if (TEST X Y) evaluates to non nil. This
is a non-destructive function; it makes a copy of SEQ if
necessary to avoid corrupting the original SEQ.

Keywords supported:  :key :count :from-end
\n(fn X SEQ [KEYWORD VALUE]...)"
  (copy-if (bind test x 1) cl-seq :key key :count count :from-end from-end))
;;(copy 3 '(1 2 3 4) :key #'1+ :test #'<)

(defun select (sequence predicates &key key)
  "Return a list of sequences, matching PREDICATES on SEQUENCE.
The Nth sequence in the result are the elements in SEQUENCE that
satisfy then Nth predicate in PREDICATES. A last element in the
result is a sequence with elements that do not match any of
PREDICATES."
  (loop for p in (push-back (apply #'not-disjoin predicates) predicates)
	collect (copy-if p sequence :key key)))

(cl-defun rotate (sequence &optional (n 1))
  "Returns a list that is LIST rotated N times."
  (nrotate-list (copy-list list) n))

(cl-defun positions-if (predicate sequence &key from-end (start 0) end key)
  (loop for start* = start then (1+ pos)
	for pos = (cl-position-if predicate sequence
		    :from-end from-end :start start* :end end :key key)
	while pos collect pos))
;;(positions-if #'oddp (vector 0 1 2 3 4 5) :start 2)

(defun split-at-position (sequence &rest positions)
  "Split SEQUENCE at POSITIONS and return the resulting subsequences as a list."
  (loop for position in (nreverse (cons (length sequence) (nreverse positions)))
	for start = 0 then end
	for end = position
	collect (subseq sequence start end)))
;;(split-at-position "qweqwe" 2 4)

(defun split-if (predicate sequence &rest args)
  "Slits SEQUENCE into two at the positions where unary PREDICATE is true."
  (apply #'split-at-position sequence (apply #'positions-if predicate sequence args)))
;;(split-if #'oddp '(1 2 3 4 5))

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

(cl-defun randomize (sequence)
  "Return the elements of SEQUENCE in random order."
  (as-list (l sequence)
    (loop repeat (length sequence)
	  collect (draw-random sequence))))
;;(randomize "Mats Bergstr;m")

(cl-defun positions (x sequence &key (test #'eql) (key #'identity))
  "Return the positions of the elements in SEQUENCE that occur in X.
X might be an atom or a sequence.

Keywords supported: :test :key"
  (let ((list (listify x)))
    (loop for y in (if key (map 'list key sequence) (coerce sequence 'list)) 
	  for i from 0
	  if (cl-member y list :test test)
	  collect i)))
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

(cl-defun nminimum-nokey (vec test)
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
;;(nminimum-nokey (vector 17 17) #'>=)

(cl-defun minimum (cl-seq &key (test #'<) key (start 0) end from-end)
  "Find the minimum of SEQ.
The returned object is the value triple (ELEMENT POSITION VALUE)
where ELEMENT is the minimum element in SEQ and POSITION is the
positition of that element in SEQ. VALUE is an element identical
to (funcall KEY ELEMENT). The default values of the supported
keywords TEST, KEY, AND FROM-END are #'<, #'IDENTITY, and NIL,
respectively.

\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (when (plusp (length cl-seq))
    (if from-end
      (minimum (nreverse cl-seq) :test test :key key)
      (destructuring-bind (min pos)
	  (nminimum-nokey
	   (if key
	     (map 'vector key (subseq cl-seq start end))
	     (coerce (subseq cl-seq start end) 'vector))
	   test)
	(let ((pos* (+ pos start)))
	  (values (elt cl-seq pos*) pos* min))))))
;;(minimum '(1 2 3 5 5) :start 3)

(cl-defun min-position (&rest args)
  "Returns position of the minimum element in SEQUENCE.
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (second (apply #'minimum args)))
;;(min-position '(1 2 1 3) :key #'1+ :from-end t)

(cl-defun min-element (sequence &rest args)
  "Returns minimum element in SEQUENCE
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (first (apply #'minimum sequence args)))
;;(min-element '())
;;(min-element '(1 2 1 3) :key #'1+)

(cl-defun min-value (sequence &rest args)
  "Returns minimum value in SEQUENCE
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (third (apply #'minimum sequence args)))
;;(min-value '(1 2 1 3) :key #'1+)
;;(min-value '())

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

(defun center (sequence k &optional right)
  "Return SEQUENCE's central subsequence of length K.
If the parity of SEQUENCE's length and K differs, the result
cannot be perfectly centered. By default, the returned center
will be offset to the left:

\(center \"01234\" 2\) ==> \"12\"

If optional parameter RIGHT is not nil, the center will be offset
to the right:

\(center \"01234\" 2 t\) ==> \"23\""
  (let ((n (length sequence)))
    (if (> k n)
      (error "Required center too long for sequence argument")
      (let ((a (first (funcall (if right #'cl-ceiling #'cl-floor) (- n k) 2))))
	(subseq sequence a (+ a k))))))
;;(loop with s = "01234" for k to (length s) collect (center s k t))

(defun assoc-project (sequence a b)
  "Map each sequence element ELT of SEQUENCE to (cons (elt ELT A) (elt
ELT B))."
  (mapcar #'(lambda (x) (cons (elt x a) (elt x b))) sequence))
;;(assoc-project '((a aa aaa) (b bb bbb)) 0 1)

(defun project-1 (sequence x)
  "Apply projection object X on SEQUENCE.
X is either an integer or a function. If X is an integer, the
function returns the Xth element of SEQUENCE. Otherwise, X must
be a function, and the function will return (funcall X SEQUENCE)."
  (if (integerp x)
    (elt sequence x)
    (funcall x sequence)))

(defun project (sequence projection)
  "Project SEQUENCE according to PROJECTION.

PROJECTION is either a projection element, a sequence of
projection object, or T. See project-1 for a description of a
projection object.

If PROJECTION is a single projection object the function will
return the result of applying the projection object on SEQUENCE.

If PROJECTION is a sequence of projection objects X1, X2, ... the
function will return the sequence Y1, Y2, ..., where YI is the
result of applying projection XI on SEQUENCE.

The resulting sequence will be of the same sequence type as
SEQUENCE.

Finally, PROJECTION might also be T. In this case the function
returns SEQUENCE unaltered."
  (if (eql projection t)
    sequence
    (if (or (integerp projection) (functionp projection))
      (project-1 sequence projection)
      (coerce (loop for x in (coerce projection 'list)
		    collect (project-1 sequence x))
	      (type-of-super sequence)))))
;;(project '(a b c) '(1 0 2 1))
;;(project '(a b c) t)
;;(project "abc" 2)
;;(project "abc" '(2))
;;(project '(0 1 2 3) #'(lambda (x) (elt x 2)))

(defun project-sequence (sequence projection)
  "Project multi-dimensional SEQUENCE according to PROJECTION-ARGS."
  (coerce (mapcar #'(lambda (x) (project x projection)) sequence)
	  (type-of-super sequence)))
;;(project-sequence '("012" "098") '(0 2))
;;(mapcar #'first (project-sequence '((1 2 3) (4 5 6)) '(1 2)))
;;(project-sequence '((1 "Mats" 'qwe) (2 "Ludvik" 'ewq)) 1)

(cl-defun insert-sequence (seq1 seq2 &key (start1 0) (end1 start1) (start2 0) end2)
  "Insert the elements in SEQ2 into SEQ1."
  (cl-flet ((is (x y z)
	      (if (listp seq1) (append x y z) (concatenate (type-of x) x y z))))
    (is (subseq seq1 0 start1) (subseq seq2 start2 end2) (subseq seq1 end1))))
;;(insert-sequence "15010" "_" :start1 2)
;;(insert-sequence '(1 2 5) '(3 4) :start1 2 :end1 2)
;;(insert-sequence '(0 1 2 3 4) '(a b) :start1 1 :end1 3 :start2 1)

(cl-defun replace-sequence (seq1 seq2 &optional (start 0) end)
  "Replace the elements in SEQ2 into SEQ1."
  (insert-sequence seq1 seq2 :start1 start :end1 (or end (+ start (length seq2)))))
;;(replace-sequence "01234" "ab" 1)
;;(replace-sequence '(0 1 2 3 4) '(a b) 1 -2)

(provide 'mb-sequences)
