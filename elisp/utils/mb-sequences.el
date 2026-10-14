;;; mb-sequences.el --- General sequence utilities -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'macroexp)

(defun mb-sequences--plist-without (plist keys)
  "Return PLIST without entries whose keys occur in KEYS."
  (cl-loop for (key value) on plist by #'cddr
           unless (memq key keys)
           append (list key value)))

(defun type-of-super (sequence)
  "Return a `cl-coerce' type specifier for SEQUENCE."
  (if (listp sequence)
      'list
    (type-of sequence)))

(defmacro random-elt (sequence)
  "Return a random element of SEQUENCE, evaluating SEQUENCE once."
  (declare (debug (form)))
  (macroexp-let2 nil value sequence
    `(elt ,value (random (length ,value)))))

(defun remove-nth (n sequence)
  "Return SEQUENCE without the element at index N."
  (cl-concatenate (type-of-super sequence)
                  (cl-subseq sequence 0 n)
                  (cl-subseq sequence (1+ n))))

(defun remove-nths (positions sequence)
  "Return SEQUENCE without the elements at POSITIONS.
POSITIONS may be in any order; duplicate positions are ignored."
  (let ((positions (delete-dups (sort (copy-sequence positions) #'<))))
    (cl-loop for element in (append sequence nil)
             for index from 0
             unless (memq index positions)
             collect element into result
             finally return (cl-coerce result (type-of-super sequence)))))

(cl-defun length* (sequence &key start end)
  "Return the length of the part of SEQUENCE between START and END."
  (- (or end (length sequence)) (or start 0)))

(defun sequence-type (sequence)
  "Return the general sequence type of SEQUENCE."
  (cond ((listp sequence) 'list)
	((vectorp sequence) 'vector)
	((stringp sequence) 'string)
        (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

(cl-defmacro as-list ((list sequence) &body body)
  "Evaluate body with SEQUENCE treated as a list.
SEQUENCE is coerced to a list bound to the symbol LIST. The
evaluation of the last form in BODY is expected to be a list,
which is finally coerced to the same type as SEQUENCE."
  `(let ((,list (cl-coerce ,sequence 'list)))
     (cl-coerce (progn ,@body) (sequence-type ,sequence))))
(put 'as-list 'lisp-indent-function 1)

(cl-defun copy-if (cl-pred cl-seq &key key count from-end)
  "Return a copy of CL-SEQ containing items satisfying CL-PRED.
At most COUNT matching items are retained, counting from the end
when FROM-END is non-nil.

Keywords supported:  :key :count :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (let* ((items (append cl-seq nil))
         (input (if from-end (reverse items) items))
         (remaining (or count most-positive-fixnum))
         result)
    (while (and input (> remaining 0))
      (let* ((element (pop input))
             (value (if key (funcall key element) element)))
        (when (funcall cl-pred value)
          (push element result)
          (setq remaining (1- remaining)))))
    (cl-coerce (if from-end result (nreverse result))
               (type-of-super cl-seq))))

(cl-defun copy (x cl-seq &key (test #'eql) key count from-end)
  "Return a copy of SEQ containing exactly the items matching X.
An element Y matches X if (TEST X Y) evaluates to non nil. This
is a non-destructive function; it makes a copy of SEQ if
necessary to avoid corrupting the original SEQ.

Keywords supported: :test :key :count :from-end
\n(fn X SEQ [KEYWORD VALUE]...)"
  (copy-if (lambda (element) (funcall test x element))
           cl-seq :key key :count count :from-end from-end))

(cl-defun select (sequence predicates &key key)
  "Return a list of sequences, matching PREDICATES on SEQUENCE.
The Nth sequence in the result are the elements in SEQUENCE that
satisfy the Nth predicate in PREDICATES.  A last element in the
result is a sequence with elements that do not match any of
PREDICATES."
  (let ((fallback
         (lambda (element)
           (not (cl-some (lambda (predicate)
                           (funcall predicate element))
                         predicates)))))
    (mapcar (lambda (predicate)
              (copy-if predicate sequence :key key))
            (append predicates (list fallback)))))

(cl-defun rotate (sequence &optional (n 1))
  "Return a copy of SEQUENCE rotated N places to the left."
  (if (zerop (length sequence))
      (copy-sequence sequence)
    (let ((split (mod n (length sequence))))
      (cl-concatenate (type-of-super sequence)
                      (cl-subseq sequence split)
                      (cl-subseq sequence 0 split)))))

(cl-defun positions-if
    (predicate sequence &key from-end (start 0) (end (length sequence)) key)
  "Return indices whose elements satisfy PREDICATE.
Restrict the search with START and END.  When supplied, END must
be an integer.  FROM-END reverses the order of the returned positions."
  (cl-loop for index from start below end
           for element = (elt sequence index)
           when (funcall predicate (if key (funcall key element) element))
           collect index into positions
           finally return (if from-end (nreverse positions) positions)))

(cl-defun split2 (x sequence &rest args)
  "Split SEQUENCE in two at the first occurrence of X.
For ARGS, see `cl-position'"
  (let ((position (apply #'cl-position x sequence args)))
    (if position
        (split-at-position sequence position)
      (list sequence nil))))

(cl-defun split-at-position (sequence &rest positions)
  "Split SEQUENCE at POSITIONS and return the resulting subsequences as a list."
  (cl-loop for position in (append positions (list (length sequence)))
	for start = 0 then end
	for end = position
	collect (cl-subseq sequence start end)))

(cl-defun split-if (predicate sequence &rest args)
  "Split SEQUENCE at positions where unary PREDICATE is true."
  (apply #'split-at-position sequence (apply #'positions-if predicate sequence args)))

(cl-defun elt-random (sequence)
  "Return a random element in SEQUENCE."
  (elt sequence (random (length sequence))))

(cl-defmacro draw-nth (n sequence)
  "Draw the Nth element from SEQUENCE.
DRAWING is an operation where one or more elements are returned
from a sequence and where these elements are deleted from the
sequence as a side effect:
\n  (let ((s \\='(a b c))) (list (draw \\='c s) s)) ==> (c (a b))
\n  (let ((s \\='(a b c))) (list (draw-nth 1 s) s)) ==> (b (a c))"
  `(prog1 (elt ,sequence ,n)
     (setf ,sequence (remove-nth ,n ,sequence))))

(cl-defmacro draw-random (sequence &optional (n 1))
  "Draw N random elements from SEQUENCE.
See `draw-nth' for a definition of the draw operation."
  (declare (debug (form &optional form)))
  (let ((index (make-symbol "index")))
    `(cl-loop repeat ,n
              for ,index = (random (length ,sequence))
              collect (draw-nth ,index ,sequence))))

(cl-defmacro draw (x cl-seq &rest cl-keys)
  "Draw all elements from CL-SEQ that match X.
Return the matching elements and remove them from CL-SEQ.
\nKeywords supported:  :test :key :count :start :end :from-end
\n(fn ITEM SEQUENCE [KEYWORD VALUE]...)"
  (let ((test (or (plist-get cl-keys :test) '#'eql))
        (keys (mb-sequences--plist-without cl-keys '(:test)))
        (element (make-symbol "element")))
    `(draw-if (lambda (,element) (funcall ,test ,x ,element))
              ,cl-seq ,@keys)))

(cl-defmacro draw-if (predicate sequence &rest cl-keys)
  "Draws all elements matching PREDICATE from SEQUENCE.
A DRAW operation is a sequence modifying operation. It deletes
the matching elements from the sequence, and returns those
elements.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn SEQUENCE PREDICATE [KEYWORD VALUE]...)"
  (let ((selection-keys (mb-sequences--plist-without
                         cl-keys '(:start :end))))
    `(prog1
         (copy-if ,predicate
                  (cl-subseq ,sequence
                             (or ,(plist-get cl-keys :start) 0)
                             ,(plist-get cl-keys :end))
                  ,@selection-keys)
       (setf ,sequence
             (cl-delete-if ,predicate ,sequence ,@cl-keys)))))

(cl-defun randomize (sequence)
  "Return the elements of SEQUENCE in random order."
  (let ((remaining (copy-sequence sequence)))
    (cl-coerce (draw-random remaining (length remaining))
               (type-of-super sequence))))

(cl-defun positions (x sequence &key (test #'eql) (key #'identity))
  "Return the positions of the elements in SEQUENCE that occur in X.
X might be an atom or a sequence.

Keywords supported: :test :key"
  (let ((items (cond
                ((stringp x) (list x))
                ((listp x) x)
                ((sequencep x) (cl-coerce x 'list))
                (t (list x)))))
    (cl-loop for y in (if key
			(cl-map 'list key sequence)
			(cl-coerce sequence 'list)) 
	  for i from 0
	  if (cl-member y items :test test)
	  collect i)))

(cl-defun minimums (cl-seq cl-pred &rest cl-keys)
  "Find all the minimum objects in SEQ according to PREDICATE.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (let ((key (plist-get cl-keys :key))
        best-value
        minima
        found)
    (cl-loop for element in (append cl-seq nil)
             for value = (if key (funcall key element) element)
             do (cond
                 ((not found)
                  (setq found t
                        best-value value
                        minima (list element)))
                 ((funcall cl-pred value best-value)
                  (setq best-value value
                        minima (list element)))
                 ((not (funcall cl-pred best-value value))
                  (push element minima))))
    (nreverse minima)))

(cl-defun nminimums (cl-seq cl-pred &rest cl-keys)
  "Compatibility alias for `minimums'.
CL-SEQ is not modified.
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (apply #'minimums cl-seq cl-pred cl-keys))
(make-obsolete 'nminimums 'minimums "2026-10-14")

(cl-defun minimum (cl-seq &key (test #'<) key (start 0) end from-end)
  "Find the minimum of SEQ.
The returned object is the value triple (ELEMENT POSITION VALUE)
where ELEMENT is the minimum element in SEQ and POSITION is the
position of that element in SEQ. VALUE is an element identical
to (funcall KEY ELEMENT). The default values of the supported
keywords TEST, KEY, and FROM-END are `<', `identity', and nil,
respectively.

\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (let ((end (or end (length cl-seq)))
        best-element
        best-position
        best-value
        found)
    (cl-loop for position from start below end
             for element = (elt cl-seq position)
             for value = (if key (funcall key element) element)
             when (or (not found)
                      (funcall test value best-value)
                      (and from-end
                           (not (funcall test best-value value))))
             do (setq found t
                      best-element element
                      best-position position
                      best-value value))
    (when found
      (list best-element best-position best-value))))

(cl-defun min-position (&rest args)
  "Return the position of the minimum element in SEQUENCE.
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (cadr (apply #'minimum args)))

(cl-defun min-element (sequence &rest args)
  "Return the minimum element in SEQUENCE.
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (car (apply #'minimum sequence args)))

(cl-defun min-value (sequence &rest args)
  "Return the minimum value in SEQUENCE.
\nKeywords supported:  :test :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (caddr (apply #'minimum sequence args)))

(cl-defun subseq* (seq &optional (start 0) (end nil))
  "Return the subsequence of SEQ from START to END.
This compatibility function is obsolete; use `cl-subseq' instead."
  (unless (null seq)
    (cl-subseq seq start end)))
(make-obsolete 'subseq* 'cl-subseq "2026-10-14")

(defun mb-sequences--pairs (list)
  "Return the consecutive overlapping pairs of LIST.
This is a local counterpart of `pairs' in `mb-lists', which is not
yet loadable outside the startup environment."
  (cl-loop for (left right) on list while right collect (list left right)))

(cl-defun deltas (sequence)
  "Return the canonical deltas around the numbers in SEQUENCE.
If A, B, C are consecutive numbers, the delta for B is (- (/ (+ A C) 2) B).
The first and last deltas are flanked by repeating their neighbour."
  (let ((half-differences
         (mapcar (lambda (pair) (/ (- (cadr pair) (car pair)) 2.0))
                 (mb-sequences--pairs (cl-coerce sequence 'list)))))
    (cl-coerce (mb-sequences--pairs
                (when half-differences
                  (append (list (car half-differences))
                          half-differences
                          (last half-differences))))
               (type-of-super sequence))))

(cl-defun center (sequence k &optional right)
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
      (let* ((difference (- n k))
             (start (if right
                        (/ (+ difference 1) 2)
                      (/ difference 2))))
        (cl-subseq sequence start (+ start k))))))

(cl-defun assoc-project (sequence a b)
  "Map each element of SEQUENCE to a cons of its Ath and Bth elements."
  (mapcar (lambda (element)
            (cons (elt element a) (elt element b)))
          sequence))

(cl-defun project-1 (sequence x)
  "Apply projection object X on SEQUENCE.
X is either an integer or a function. If X is an integer, the
function returns the Xth element of SEQUENCE. Otherwise, X must
be a function, and the function will return (funcall X SEQUENCE)."
  (if (integerp x)
    (elt sequence x)
    (funcall x sequence)))

(cl-defun project (sequence projection)
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
      (cl-coerce (cl-loop for x in (cl-coerce projection 'list)
		    collect (project-1 sequence x))
	      (type-of-super sequence)))))

(cl-defun project-sequence (sequence projection)
  "Project multi-dimensional SEQUENCE according to PROJECTION-ARGS."
  (cl-coerce (mapcar (lambda (element) (project element projection)) sequence)
	     (type-of-super sequence)))

(cl-defun insert-sequence (seq1 seq2
			   &key (start1 0) (end1 start1) (start2 0) end2)
  "Insert the elements in SEQ2 into SEQ1."
  (cl-concatenate (type-of-super seq1)
                  (cl-subseq seq1 0 start1)
                  (cl-subseq seq2 start2 end2)
                  (cl-subseq seq1 end1)))

(cl-defun replace-sequence (seq1 seq2 &optional (start 0) end)
  "Replace the elements in SEQ2 into SEQ1."
  (insert-sequence seq1 seq2
		   :start1 start :end1 (or end (+ start (length seq2)))))

(provide 'mb-sequences)

;;; mb-sequences.el ends here
