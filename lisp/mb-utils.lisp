(defpackage :mb-utils
  (:use :common-lisp)
  (:export
   :average
   :remf* :popf
   :listify :list<
   :sq
   :a-b :0-n :abs- :abs+ :string-case
   :cut :cut-if
   :nor :awhen :aif :it :awhile :acond
   :mnth :melt
   :project
   :nth* :pop-list
   :rcons
   :copy-if :infix-list
   :remove-nth
   :write-list :concat :format-list :split-by-char
   :bind :mbind :newline :mapreduce
   :members :split :compose :equal-elements
   :multi-split :nsplit-list-if :nsplit-list :split-list :list-insert
   :relations
   :copy-object :copy-object-to
   :draw :draw-if
   :subseq* :last-elt :butlast* :head :last* :butfirst
   :minimum :maximum
   :transpose-tree :flatten* :maptree :with-tree
   :flank
   :group :pairs :tuples :combinations :interleave
   :boundaries
   :with-gensyms
   :with-outfile
   :read-lines :file->lines :file->string
   :write-lines :lines->file :string->file
   :read-text-file :read-text-file-lines ;;deprecated methods
   :sequence-index-boundary
   :win32-homepath
   :parse-iso-dttm :parse-iso-date :parse-iso-time :iso-time
   :tree->value-index-tuples
   :tree-dimensions
   :tree->array :array->tree
   :generate-array
   :unit-sequence
   :array-row :array-rows :array-column
   :span-array :map-array
   :map-array-rows
   :array-reverse-rows
   :swap-rows
   :random-interval :random-array
   :dimensions
   :alias
   :with-transpose
   :expand-list :expand-tree :expand-sequence
   :deltas
   :replace-nth :nreplace-nth))

(in-package :mb-utils)

(defun average (sequence &key (accumulation-fn #'+) weights (normalize-weights-p t) (weight-fn #'*))
  "Returns average of the values in SEQUENCE summing with
ACCUMULATION-FN If WEIGHTS are provided they must be of the same
length as SEQUENCE, and then the weighed average of sequence and
weighed is returned. By default weights are normalized by AVERAGE, but
the normalization can be suppressed by setting NORMALIZE-WEIGHTS-P to
nil"
  (if weights
    (reduce accumulation-fn
	    (map 'list weight-fn
		 sequence
		 (if normalize-weights-p
		   (mapcar (bind #'/ (reduce #'+ weights)) weights)
		   weights)))
    (/ (reduce accumulation-fn sequence) (length sequence))))
;;(average (a-b 1 10 :type 'vector) :weights '(0 0 0 0 0 1 1 1 1 1))

(defun remf* (plist key)
  "Returns a copy of PLIST where KEY and its associated value are removed."
  (let ((res (copy-list plist)))
    (remf res key)
    res))
;;(let ((plist '(:a 1 :b 2))) (list (remf* plist :a) plist))

(defun popf (plist key)
  "Returns getf and operates a remf."
  (prog1 (getf plist key)
    (remf plist key)))
;;(let ((plist '(:a 1 :b 2))) (list (popf plist :c) plist))

(defun sq (x) (declare (number x)) (* x x))
(defmacro nor (&rest conditions) "Nor." `(not (or ,@conditions)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun minimum (list &key (test #'<) (key #'identity))
  "Returns a minimum element m of SEQUENCE such that \(TEST (KEY x)
\(KEY m\)\) is false for every element x of SEQUENCE. Also returns the
position of m in SEQUENCE and then evaluated minimum value (KEY m) as
second and third arguments respectively."
  (loop with best = (list (funcall key (first list)) (first list) 0) ;i.e (MINIMUM-VALUE MINIMUM-ELEMENT MINIMUM-POSITION)
	for x in (rest list)
	for i from 1
	for val = (funcall key x)
	if (funcall test val (first best))
	do (setf best (list val x i))
	finally (return (values (second best) (first best) (third best)))))
;;(minimum '(3 2 1 5) :test #'> :key #'-)

(defun maximum (list &key (test #'<) (key #'identity))
  (minimum list :test (complement test) :key key))
;;(maximum '(3 2 1 5))

(defun nrcons (list x)
  "B after LIST. Destructive."
  (nconc list (list x)))
;;(let ((l '(a))) (list (nrcons l 'b) l))

(defun rcons (list x) (append list (list x)))
;;(let ((l '(1 2))) (list (rcons l 1) l))

(defun nflank (a list &optional (b a))
  "Inserts A before and B after LIST. Destructive."
  (nconc (cons a list) (list b)))
;;(let ((l (list 2 3))) (list l (nflank 1 l 4)))

(defun flank (a list &optional (b a))
  "Inserts A before and B after LIST.
If B is not specified, A is used."
   (nflank a (copy-list list) b))
;;(let ((l (list 2 3))) (list l (flank 1 l 4)))

(defmacro twins (x) `(make-list 2 :initial-element ,x))
;;(twins (+ 1 2))

(defun pairs (list &key (key #'identity) (flank-p nil))
  "Returns the elements in list as consecutive pairs.
I.e. for list \(x11 x2 x3 ... xn-1 xn\), it returns the list
\((x1 x2) (x2 x3) ... (xn-1 xn)\) See also `tuples' for a more
generalized version. If flank-p is non-nil the result is
`flank'ed with pairs of its first and last element."
  (let ((res (loop for (x y) on (mapcar key list) while y collect (list x y))))
    (if flank-p
      (nflank (twins (caar res)) res (twins (cadar (last res))))
      res)))
;;(pairs '(1 2 3 4 5) :key #'1+ :flank-p t)

(defun tuples (list n)
  "Returns the elements in list as consecutive tuples.
I.e. for list \(x11 x2 x3 x4 ... xn-2 xn-1 xn\), with N = 3, 
it returns the list \((x1 x2 x3) (x2 x3 x4) ... (xn-2 xn-1 xn)\)
TODO: implement a mapping key, see `pairs' (when needed)"
  (loop for h on list
	for i from (- (length list) n) downto 0
	collect (butlast h i)))
;;(tuples '(a b c d e) 3)

(defun combinations (list1 &optional (list2 list1) ordered)
  (nconc (loop for x1 in list1 nconc 
	       (loop for x2 in list2
		     collect (list x1 x2)))
	 (when (not ordered) (combinations list2 list1 t))))
;;(combinations '(a b c) '(d e f) t)

(defun last* (list &optional n)
  "Same as `butlast' but accepts negative argument, meaning counting from start."
  (last list (mod (or n 1) (length list))))

(defun butlast* (list &optional n)
  "Same as `butlast' but accepts negative argument, meaning counting from start."
  (butlast list (mod (or n 1) (length list))))
;;(butlast* '(a b c d e) -2)

(defun head (list &optional (n 1))
  "Returns the N first elements of LIST"
  (butlast* list (- n)))

(defun butfirst (list &optional n)
  "Returns the N first elements of LIST"
  (last* list (- n)))

(defun positions-if (predicate sequence &key from-end (start 0) end key)
  (loop for start* = start then (1+ pos)
	for pos = (position-if predicate sequence :from-end from-end :start start* :end end :key key)
	while pos collect pos))
;;(positions-if #'oddp (vector 0 1 2 3 4 5) :start 2)

(defun cut-if (predicate sequence inclusion &rest args)
  "INCLUSION not supported yet."
  (loop for start = 0 then end
	for end in (apply #'positions-if predicate sequence args)
	if (subseq sequence start end) collect it into res
	finally (return (if inclusion
			  (append res (list (subseq sequence (or end 0))))
			  (mapcar #'(lambda (x) (subseq x 1))
				  (append res (list (subseq sequence (or end 0)))))))))
;;(cut-if (constantly nil) (vector 0 1 2 3 4 5) nil)
;;(cut-if #'oddp (vector 0 1 1 2 2 2 3 4 5) nil)
;;(cut-if #'(lambda (x) (char= x #\a)) "Anaconda" nil)

(defun group-positions (list &key (test #'eq) (key #'identity))
  "Groups LIST into a list of sublists where all elements are equal
according to TEST and KEY."
  (loop for p in (pairs list :key key)	
	for i from 0
	if (not (apply test p)) collect (1+ i)))
;;(group-positions '((1 2) (1 2)) :test #'equal)
;;(equal '(1 2) '(1 2))

(defun group (list &key (test #'eq) (key #'identity))
  "Groups LIST into a list of sublists where all elements are equal
according to TEST and KEY."
  (loop for a = 0 then b
	for b in (group-positions list :test test :key key)
	collect (subseq list a b) into res
	finally (return (nconc res (list (subseq list a))))))
;;(group '(a a b a b) :test #'equal)
;;(group '(((1) (2)) ((1) (2)) ((1) (3)) ((1) (2))) :test #'equal)
;;(group '(nil nil nil) :test #'equal)

(defun accumulate-sorted-list (list &optional (test #'eql) (min-occurences 1))
  "Accumulates sorted LIST. See `accumulate-list' for details."
  (loop for x in (group list :test test)
	for l = (length x)
	if (>= l min-occurences) collect (list (first x) l)))
;;(accumulate-sorted-list '(a a a b b c d c)) ==> ((a 3) (b 2) (c 1) (d 1) (c 1))
;;(accumulate-sorted-list '(a a a b b c d c) 'equal 2)

(defun lt-equal (less-than)
  (lambda (x y)
    (or (funcall less-than x y)
	(not (funcall less-than y x)))))
;;(loop for args in '((0 1) (0 0) (1 0)) collect (apply (lt-equal #'<) args))

(defun accumulate-list (list &optional (test #'<) (min-occurences 1))
  "Returns a list of pairs (X count-X), where X is an element in
list and count-X is the number of occurrences of X. Note that the
resulting list is sorted on the value of COUNT-X"
  (accumulate-sorted-list (sort list test) (lt-equal test) min-occurences))
;;(accumulate-list '(a b c a b a d) #'symbol<) ==> ((a 3) (b 2) (c 1) (d 1))
;;(accumulate-list '(a b c a b a d) #'symbol< 2)

(defun repetitions-1-slow (list &optional (start-index 0) (test #'eql))
  "Returns a list of repetitions from first element in list"
  (loop with target-list = list
  	with cycle-length = 1
	for x in (rest list)
	for i from 2
	for match = (funcall test x (first target-list))
	for match-first = (funcall test x (first list))
	for complete-cycle = (zerop (mod i cycle-length))

	if match
	 if complete-cycle
	  collect (list start-index cycle-length) 
	  and do (setf target-list list) 
	 else do (setf target-list (rest target-list))
        else do (if match-first
		  (setf cycle-length (1- i)
			target-list (rest list))
		  (setf cycle-length i
			target-list list))))
;;(repetitions-1-slow '(a b c a b c a b c a b c))
;;(repetitions-1-slow '(a a))

(defun repetitions-slow (list &optional (test #'eql))
  "Returns a description of all repetitive patterns in LIST.
Each element is on the form ((INDEX LENGTH) COUNT), where INDEX
is the start position of pattern, LENGTH is the pattern length
and COUNT is the number of repetitions of the pattern. Note that
COUNT really counts _repetitions_, so a value of 1 means two
sucsessive occurences of the pattern.
Algorithm i O(n^2)."
  (loop for x on list 
	for i from 0
	if (repetitions-1-slow x i test)
	append (accumulate-sorted-list (repetitions-1-slow x i test) #'equal)))
;;(repetitions-slow '(a b c a b c a b c a b c))

;;TODO move this to another file
(defmacro string-case (arg &rest clauses)
  "Same as CASE, but compares with STRING-EQUAL instead of EQL.
TODO: string keys could also evalute to a list"
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (cond ,@(mapcar #'(lambda (cl)
                           (let ((k (car cl)))
                             `(,(cond ((member k '(t otherwise))
                                       t)
                                      ((consp k)
                                       `(member ,g ',k :test #'string=))
                                      (t `(string= ,g ',k)))
                               (progn ,@(cdr cl)))))
                       clauses)))))

(defun a-b (a b &key length (step (if length (/ (- b a) (1- length)) 1)) (type 'list) (direction :auto) key)
  "Returns the sequence from and including A to and including B by STEP
 If TYPE is provided, the result is #'COERCEd to TYPE."
  (assert (not (zerop step)))
  (flet ((up () (if length
		  (rcons (loop for x = a then (+ x step)
			       repeat (1- length)
			       collect (if key (funcall key x) x))
			 b)
		  (loop for i from a to b by step collect (if key (funcall key i) i))))
	 ;;TODO same as with up
	 (down () (loop for i downfrom a to b by (abs step) collect (if key (funcall key i) i))))
    (let ((res (case direction
		 (:up (up))
		 (:down (down))
		 (:auto (if (< a b) (up) (down))))))
      (values
       (if (and type (not (eql (type-of res) type)))
	 (coerce res type)
	 res)
       step))))
;;(a-b 2 3)
;;(loop for i from 2 below 20 collect (length (a-b (- pi) pi :length i)))
;;(a-b 2 -2 :length 10 :type 'vector :direction :auto :key #'sqrt)
;;(a-b 10 0 :type 'array :key #'(lambda (x) (list x (sq x))))

(defun 0-n (n &rest args)
  "Returns sequence 0...N-1. If N is negative the sequence is descending.
Use keywords to specify sequence type. See A-B for details on possible keywords."
  (unless (zerop n)
    (apply #'a-b 0 (1- (if (minusp n) (1+ n) n)) args)))
;;(mapcar #'0-n '(-1 0 1 2))

(defun abs- (x &optional (n 1))
  "Returns the number N integers closer to zero than X. If |X-N| > 0 the result is undefined."
  (- x (* (signum x) n)))
;;(loop for x from -2 to 2 collect (abs- x 2))

(defun abs+ (x &optional (n 1) (negative-if-zerop nil))
  "Returns the number N integers further away from zero than X. If x
is zero, the result is -N if NEGATIVE-IF-ZEROP is true, otherwise it
is N"
  (if (zerop x)
    (if negative-if-zerop (- n) n)
    (abs- x (- n))))
;;(loop for x from -2 to 2 collect (abs+ x 1))

(defun ncut (list &optional (n 2) (include-remainder nil))
  "Cuts LIST into sublists of length N while preserving order.
If INCLUDE-REMAINDER is nil and last element in the result list
has length shorter than N, this last element is discarded.
Destructive."
  (do* ((result nil (cons first result))
	first last)
       ((null list)
	(nreverse (if (or include-remainder
			  (not (< (length first) n)))
		    result (cdr result))))
    (setf first list)
    (setf last (nthcdr (1- n) first))
    (setf list (cdr last))
    (and last (setf (cdr last) nil))))
;;(let ((qwe '(1 2 3 4 5 6))) (values (ncut qwe) qwe))

(defun cut (list &optional (n 2) (include-remainder nil))
  "Non-destructive version of NCUT."
  (ncut (copy-list list) n include-remainder))
;;(let ((qwe '(1 2 3 4 5 6))) (values (cut qwe) qwe))

(defun interleave-sequences (sequences &optional (type (class-of (first sequences))))
  (let* ((lengths (mapcar #'length sequences))
	 (max (reduce #'max lengths))
	 (min (reduce #'min lengths)))
    (unless (apply #'>= lengths)
      (warn "Lengths of sequences increases. Some elements will not be interleaved."))
    (unless (<= (- max min) 1)
      (warn "Lengths of sequences differs more than 1. Some elements will not be interleaved."))
    (coerce (loop for i below min append
		  (loop for s in sequences collect (elt s i)))
	    type)))
;;(interleave-sequences '(#(a b c) #(d e f)))

(defun interleave (sequence &rest sequences)
  (interleave-sequences (cons sequence sequences)))
;;(interleave #(a b c) #(d e f))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defun copy-if (test sequence &rest args)
  (apply #'remove-if (complement test) sequence args))
;;(copy-if #'oddp '(1 2 3) :key #'1+)

(defun infix-list (list infix) 
  "'(a b c) => '(a INFIX b INFIX c)"
  (when list
    (let (result)
      (dolist (x (rest list) (cons (first list) (nreverse result)))
	(push infix result) (push x result)))))
;;(infix-list '(1 2 3) 'qwe)

;;;; CONCAT is dead! Long live CONCAT! I have rewritten concat so that
;;;; it now relies on the more general write-list, which writes a list
;;;; to an arbitrary stream. concat then calls this function with
;;;; stream bound to nil. This is much faster than CONCATENATEing
;;;; strings. For instance with 1000 elements old concat spent ~10
;;;; secs. The new concat spends only 0.05 secs for a list of 100.000
;;;; elements! Also, the new concat is more flexible in that the list
;;;; elements must not be string (possible after mapped with :key),
;;;; which means that the :test function can be set to take any type
;;;; as argument, and not just a string. Finally, there was a
;;;; restriction on list length in the old version due to the use of
;;;; APPLY (currently in my clisp version, the limit is 4096). There
;;;; is no such restriction in the new concat (or rather, write-list).
;;;; And INFIX-LIST is now obsolete, as far as I can see.
(defun write-list (list out &key pre in suf test key)
  (flet ((sp (x) (when x (case x (:newline (format nil "~%")) (t (format nil x))))))
    (let ((list (if key (mapcar key list) list)))
      (format out (concatenate 'string "~@[~a~]~{~a" (format nil "~@[~~^~a~]" (sp in)) "~}~@[~a~]")
	(sp pre) (if test (copy-if test list) list) (sp suf)))))
;;(time (progn (write-list (a-b 0 10000) nil :in ", " :pre "<<" :suf ">>" :test #'oddp :key #'1+) :fine))
;;(write-list (a-b 0 10) nil :in :newline :pre "<<" :suf ">>" :test #'oddp :key #'1+)

(defun concat (list &rest args) (apply #'write-list list nil args))
;;(time (progn (concat (a-b 0 100000) :in ", " :pre "<<" :suf ">>" :test #'oddp :key #'1+) :fine))

(defun format-list (out list format-fn &key pre in suf test key)
  (flet ((sp (x) (when x (case x (:newline (format out "~%")) (t (princ x out))))))
    (let ((list (if key (mapcar key list) list)))
      (sp pre)
      (loop for x on (if test (remove-if-not test list) list)
           do (funcall format-fn out (first x))
           while (rest x) do (sp in))
      (sp suf))))
;;(format-list t '(1 2 3) #'(lambda (out x) (format out "~a" x)) :in ", ")

(defun split-by-char (string char &optional remove-empty-string-p)
   "Returns a list of substrings of string divided by ONE CHAR each.
Note: Two consecutive spaces will be seen as if there were an empty
string between them."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i :test #'char=)
	  as s = (subseq string i j)
          if (not (and remove-empty-string-p (string= s ""))) collect s
          while j))
;;(split-by-char (nth 5 (read-text-file-lines testfile)) #\Space t)

(defun string->lines (string &optional remove-empty-lines-p (split-char #\Newline))
  "Splits string to lines"
  (split-by-char string split-char remove-empty-lines-p))
;;(string->lines (format nil "a~%b~%~%c~%"))

(defun bind (function fixed-argument &optional (floating-argument-position 0))
  (let ((f function)
	(a fixed-argument)
	(pos floating-argument-position))
    (function 
     (lambda (x)
       (if (= pos 0)
	 (funcall f x a)
	 (funcall f a x))))))

(defun listify (x &optional (atom-test #'atom))
  (if (funcall atom-test x) (list x) x))

(defun remove-nth-list (positions list &optional inverse)
    "Remove elements in SEQUENCE at POSITIONS.
Same as REMOVE-NTH, but for lists only."
  (let ((positions (listify positions)))
    (loop for x in list
	  for i from 0
	  when (eql (not (null (find i positions))) inverse)
	  collect x)))
;;(remove-nth-list '(1) '(a b c d e) t)

(defun remove-nth (positions sequence &optional inverse)
  "Remove elements in SEQUENCE at POSITIONS.
POSITIONS can be either an integer or a list of integers. If INVERSE
is not NIL, it removes elements that are not in POSITIONS"
  (coerce (remove-nth-list positions (coerce sequence 'list) inverse)
	  (class-of sequence)))
;;(remove-nth '(1 2) #(a b c) nil)
;;(remove-nth '(1 2) "abcd" t)

(defun mbind-normalize-arguments (bound-arguments &optional bound-argument-positions)
  ";;listify
 (mbind-destruct '(qwe) (1)) --> ((qwe) (1))
 (mbind-destruct 'qwe 1) --> ((qwe) (1))
 (mbind-destruct '(qwe ewq) 1) --> ((qwe) (1 2))
 ;;resolve optional
 (mbind-destruct '(qwe)) --> ((qwe) (1))
 (mbind-destruct '(qwe ewq)) --> ((qwe) (1 2))"
  (if (atom bound-arguments)
    (mbind-normalize-arguments (listify bound-arguments) bound-argument-positions)
    (if (null bound-argument-positions)
      (mbind-normalize-arguments bound-arguments 1)
      (if (atom bound-argument-positions)
	(if (integerp bound-argument-positions)
	  (list bound-arguments (a-b bound-argument-positions (length bound-arguments)))
	  (error "bound-argument-positions should be an integer or a list of integers"))
	(if (every #'integerp bound-argument-positions)
	  (if (= (length bound-arguments) (length bound-argument-positions))
	    (list bound-arguments bound-argument-positions)
	    (error "Lists bound-arguments and bound-argument-positions should have the same length"))
	  (error "bound-argument-positions should be an integer or a list of integers"))))))
;;(mbind-normalize-arguments '(qwe ewq))

(defmacro mbind (function bound-arguments &optional bound-argument-positions)
  (destructuring-bind (arguments positions)
      (mbind-normalize-arguments bound-arguments bound-argument-positions)
    (let ((gensyms (loop repeat (1+ (apply #'max positions)) collect (gensym))))
      `(let ,(loop for pos in positions
		   for arg in arguments
		   collect (list (nth pos gensyms) arg))
	 (lambda (,@(remove-nth positions gensyms) &rest args)
	   (apply ,function ,@gensyms args))))))
;;(macroexpand-1 '(mbind #'- (123 321) (1 2)))
;;(macroexpand-1 '(mbind #'integrate-simpson-sequence ((- (/ pi 2)) (/ pi 2))))
;;(mapcar (mbind #'- (123 321) (1 2)) '(1 2 3) '(1 2 3))
;;(mapcar (mbind #'- 0) (a-b 0 10) (a-b 0 10))

(defun newline (&optional (n 1))
  (make-string n :initial-element #\newline))

(defun mapreduce (function lists)
  (mapcar #'(lambda (x) (reduce function x)) lists))
;;(mapreduce #'+ '((1 2)))

(defun members (items sequence &rest args)
  "Same as member, but tests true if one of the ITEMS is in SEQUENCE"
  (member items sequence :test #'(lambda (x y) (apply #'member y x args))))
;;(members '(0) (members '(0) '(1 1 0 1 1 0 1)) :test-not #'eql)

(defun split-to-be-deleted (sequence split-items)
  (loop for temp = 0 then end
	for start = (and temp (position-if #'(lambda (x) (not (find x split-items))) sequence :start temp))
	for end =  (and start (position-if #'(lambda (x) (find x split-items)) sequence :start start))
	while start
	collect (subseq sequence start end)))

(defun split (sequence split-items)
  (loop for start = 0 then (and end (1+ end))
	for end = (and start (position-if #'(lambda (x) (find x split-items)) sequence :start start))
	while start collect (subseq sequence start end)))
;;(split '(1 1 0 0 1 1 0 1) '(0))
;;(split "abcde" "c")

;;; move this to utils later
(defmacro draw (elt sequence &rest cl-args)
  "Returns all occurences of ELT in SEQUENCE and likewise deletes these elements in SEQUENCE.
Hence this is a very setf-like and destructive function."
  `(prog1 (make-list (count ,elt ,sequence ,@cl-args) ,elt)
     (setf ,sequence (delete* ,elt ,sequence ,@cl-args))))
;;(let ((l (list 1 2 3 1 4))) (list (draw l 1) l))

(defmacro draw-if (predicate sequence &rest cl-args)
  "Returns all occurences in SEQUENCE that matches PREDICATE and likewise deletes these elements in SEQUENCE.
Hence this is a very setf-like and destructive function."
  `(prog1 (copy-if ,predicate ,sequence ,@cl-args)
     (setf ,sequence (delete-if ,predicate ,sequence ,@cl-args))))
;;(let ((l (list 1 2 3 1 4))) (list (draw-if #'oddp l) l))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns 
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun equal-elements (sequence &rest args)
  (or (zerop (length sequence))
      (not (apply #'find (elt sequence 0) sequence :test (complement (or (getf args :test) #'eql)) args))))
;;(equal-elements (append (list 1) (make-list 10000000)))

(defun multi-split (x split-function &optional split-points)
  "Splits X at SPLIT-POINTS a list of ascending scalars, using SPLIT-FUNCTION for each split."
  (if split-points
    (multiple-value-bind (left right) (funcall split-function x (first split-points))
      (if right
	(cons left (multi-split right split-function (rest split-points)))
	(list left)))
    (list x)))
;;(mapcar #'geometry:boundary (multiple-value-list (geometry:nsplit (geometry:make-interval 0 10) 2)))

;;; These are general list utils and should be moved
(defun nsplit-list-if (predicate list &key (key #'identity))
  "Returns two lists: ELEMENTS-TO-MATCH and ELEMENTS-FROM-MATCH. In
addition it returns \(last ELEMENTS-UNTIL-MATCH), as a bonus. It can
be useful to have this pointer ready in some cases, for efficiency.
This function is a generalization MEMBER-IF since The second value it
returns is the same as the return value of MEMBER-IF."
  (if (funcall predicate (funcall key (first list)))
    (values nil list nil)
    (loop for l on list
		     for r on (rest l)
		     if (funcall predicate (funcall key (first r)))
		     do (setf (cdr l) nil) and
		     do (return (values list r l))
		     ;; If no split point found, return trivial result
		     finally (return (values list nil nil)))))
;;(let ((l '(7))) (list (nsplit-list-if (bind #'> 100) l) l))

(defun nsplit-list (item list &key (key #'identity) (test #'eql))
  "The single element compare version of nsplit-list-if."
  (nsplit-list-if #'(lambda (x) (funcall test item x)) list :key key))
;;(nsplit-list 3 '(1 2 3 4 5) :test #'<)

(defun split-list (item list &key (key #'identity) (test #'eql))
  "The single element compare version of nsplit-list-if."
  (nsplit-list item (copy-list list) :key key :test test))
;;(split-list 30 '(1 2 3 4 5) :test #'<)
;;(position 3 '(1 2 3 4 5) :test #'<)

(defun transpose-tree (x)
  "The classic Lisp transpose!
TODO: Move to utils"
  (when x (apply #'mapcar #'list x)))
;;(transpose-tree '((l0 r0) (l1 r1) (l2 r2)))

;;; copy
;;(defclass foo () ((a :accessor foo-a :initarg :a)))
;;(defclass foobar (foo) ((b :accessor foobar-b :initarg :b)))
;;(defun foobar-list (x) (list (foo-a x) (foobar-b x)))
;;(setf x (make-instance 'foobar :a 12 :b 21))
;;(setf y (make-instance 'foo :a 13))
(defun copy-object-initarg-plist (x &rest supersede-plist)
  "Helper function for the copy-object macro. It returns a complete
plist for MAKE-INSTANCE, based on the metaobject of class of X. Each
pair in plist is on the form INITARG VALUE, where VALUE is the current
slot value in X for the slot that corresponds to INITARG. If
SUPERSEDE-PLIST also contains a INITARG symbol, the following value
will superesede the value from X."
  #+clisp
  (loop for metaslot in (clos:class-slots (find-class (type-of x)))
	for slot-name = (clos:slot-definition-name metaslot)
	for slot-initarg = (first (clos:slot-definition-initargs metaslot))
	for slot-value = (getf supersede-plist slot-initarg (slot-value x slot-name))
	collect slot-initarg
	collect slot-value)
  #+sbcl
  (loop for metaslot in (sb-mop:class-slots (find-class (type-of x)))
	for slot-name = (sb-mop:slot-definition-name metaslot)
	for slot-initarg = (first (sb-mop:slot-definition-initargs metaslot))
	for slot-value = (getf supersede-plist slot-initarg (slot-value x slot-name))
	collect slot-initarg
	collect slot-value)
  #-(or clisp sbcl) (error "Not implemented"))
;;(copy-object-initarg-plist x :a 1 :b nil)

(defun copy-object (x &rest supersede-plist)
  "Shallow-copies object X, i.e. returns a new object of the same
class and EQL slotvalues as X is returned. SUPERSEDE-PLIST can be used
to overstyre the copying of some of the slots. SUPERSEDE-PLIST is a
list on the form (INITARG-1 VALUE-1 INITARG-2 VALUE-2 ...) where
INITARGS are the initargs set in the classdef of the class of X. The
VALUEs are the corresponding values. In effect, using SUPERSEDE-PLIST
is a shortcut for copying and then SETFing slot values.

\(copy-object x :a 1) == (progn (copy-object x) (setf (slot-value x a) 1))

Note: For shallow-copy, this is indeed only a shortcut. But for
deep-copy (if such a method is ever written), it could be really time
saving. For instance, if x contains a slot, elements, that contains
tons of data, and we only want to copy the rest X

\(copy-object x :elements nil)

is not only a code shortcut, but also saves time and memory."
  (when x
    (apply #'make-instance (type-of x)
	   (apply #'copy-object-initarg-plist x supersede-plist))))
;;(macroexpand-1 '(copy-object x (a 123)))
;;(foobar-list (copy-object x :a 123))
;;(foobar-b x)

(defun copy-object-to (x type &rest supersede-plist)
  "Same as copy-object, but the result can be of another compatible
type, typically a subclass of X's class. See copy-object for details
about the copying process.
Note! If TYPE is a class with slots without :initform, this method fails.
Perhaps this should be handled?"
  (apply #'copy-object (apply #'make-instance type (copy-object-initarg-plist x)) supersede-plist))
;;(macroexpand-1 (foobar-list (copy-object-to y 'foobar :a 321 :b 123))
;;(copy-object-initarg-plist y :b 14)

(defmacro subseq* (sequence start &optional end)
  (with-gensyms (glength)
    `(let ((,glength (length ,sequence)))
       (subseq ,sequence (mod ,start ,glength) (and ,end (mod ,end ,glength))))))
;;(subseq* (a-b 0 10) 1 -1)

(defun last-elt (sequence &optional (n 1))
  (elt sequence (- (length sequence) n)))
;;;;(loop for x in (list "qwe" '(0 1 2) #(3 4 5)) collect (last-elt x 1))

(defun flatten* (x &optional (levels most-positive-fixnum))
  "Flattens out all arguments in tree X. If optional LEVELS is a
number, only flatten down this many tree levels."
  (if (atom x)
    (list x)
    (if (plusp levels)
      (mapcan (bind #'flatten* (1- levels)) x)    
      x)))

(defun maptree (function tree &optional (levels most-positive-fixnum))
  "Maps TREE to another three with same structure applying
FUNCTION If optional argument LEVEL is provided, the mapping goes
only this number deep in TREE. If LEVEL is 1 the method is
similar to `mapcar'"
  (if (and (listp tree) 
	   (plusp levels))
    (loop for x in tree
	  collect (maptree function x (1- levels)))
    (funcall function tree)))
;;(maptree #'1+ '(1 2 (3 4)))

(defmacro with-tree ((var array) &body body)
  "Transforms ARRAY a tree, binds this to VAR in BODY and returns the array transform of BODY result."
  `(let ((,var (array->tree ,array)))
     (tree->array (progn ,@body))))
;;(with-tree (x #2A((1 2 3) (a b c))) (rest x))

(defun nth* (n list)
  "Same as NTH, but accepts negative arguments. If N < 0 then (nth* N
  LIST) returns the Nth last element in LIST. In fact, if L is the
  length of LIST, it returns always the Mth element in LIST, where 0
  <= M < L and M is equal to N modulo L."
  (nth (mod n (length list)) list))
;;(nth* -1123 '(1 2 3))

(defun mnth (list &rest positions)
  "Extracts the elements at POSITIONS from LIST"
  (loop for i in positions collect (nth i list)))
;;(mnth (0-n 10) 5 2 7)

(defun melt (sequence &rest positions)
  "Extracts the elements at POSITIONS from SEQUENCE"
  (coerce (apply #'mnth (coerce sequence 'list) positions) (class-of sequence)))
;;(melt (0-n 10 :type 'vector) 5 2 7)

(defun project-list (tree &rest positions)
  (mapcar #'(lambda (x) (apply #'mnth x positions)) tree))
;;(project-list '((a b c) (d e f)) 0 2)

(defun project (seq-tree &rest positions)
  "SEQ-TREE is a sequence of sequences"
  (coerce (loop for x in (coerce seq-tree 'list)
		collect (apply #'melt x positions))
	  (class-of seq-tree)))
;;(project #(#(a b c) (d e f)) 0 2)

(defun list-insert (x n list)
  "Inserts element X at position N in LIST"
  (if (zerop n)
     (push x list)
     (push x (cdr (nthcdr (1- n) list)))))
;;(let ((qwe '(0 1 2 3))) (list (list-insert 'a 3 qwe) qwe))

(defmacro pop-list (place &optional (n 1) (reverse nil))
  "Pops N times from list at PLACE and returns all the popped
elements. The order is maintained in the returned list unless REVERSE
is true. The latter option is the fastest in this implementation."
  (let ((glist (gensym)))
    `(let ((,glist ()))
       (dotimes (i ,n)
	 (when ,place (push (pop ,place) ,glist)))
       (if ,reverse ,glist (nreverse ,glist)))))
;;(let ((qwe '(1 2 3 4))) (list (pop-list qwe 2) qwe))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro with-outfile ((out filespec &key (if-exists :supersede)) &body body)
  `(with-open-file (,out ,filespec :direction :output :if-exists ,if-exists)
     ,@body))

(defun make-temporary-file (&optional (prefix "/tmp/tmp"))
  #+clisp
  (posix:mkstemp prefix)
  #+sbcl
  (declare (ignore prefix))
  (error "Not implemented")
  #-(or clisp sbcl)
  (error "Not implemented"))

(defmacro with-temporary-file ((stream &optional prefix) &rest body)
  "Executes BODY with STREAM bound to a file stream to a newly created
file. Returns the pathname of the stream together with the value of last form in BODY."
  `(let ((,stream (make-temporary-file ,(or prefix "/tmp/tmp"))))
     (values (pathname ,stream)
	     (prog1 (progn ,@body) (close ,stream)))))
;;; read text
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
;;(file->lines "/home/MBe/projects/imms/data/rao/txt/RAO_FR85_LC78.txt" :end 1)

(defun read-text-file-lines (&rest args)
  (warn "READ-TEXT-FILE-LINES is deprecated. Use FILE->LINES instead.")
  (apply #'file->lines args))
;;(first (read-text-file-lines "/home/MBe/projects/imms/data/rao/txt/RAO_FR85_LC78.txt" t))

(defun file->string (path &rest args)
  (concat (apply #'read-text-file-lines path args) :in (string #\Newline)))

(defun read-text-file (&rest args)
  (warn "READ-TEXT-FILE is deprecated. Use FILE->STRING instead.")
  (apply #'file->string args))

;;; write text
(defun write-lines (lines stream)
  (loop for line in lines do (write-line line stream)))
;;(write-lines '("line1" "line2") t)

(defun lines->file (lines filespec &key (if-exists :supersede))
  (with-open-file (out filespec :direction :output :if-exists if-exists)
    (write-lines lines out)))
;;(let ((path "~/tmp/lines.txt")) (lines->file '("line1" "line2") path) (file->string path))

(defun string->file (string filespec &key (if-exists :supersede))
  (with-open-file (out filespec :direction :output :if-exists if-exists)
    (write-string string out)))
;;(let ((path "~/tmp/lines.txt")) (string->file "qwe" path) (file->string path))

(defun neq (x y) (not (eq x y)))

(defun relations (list &optional with-identity ordered)
  "Returns a list containing all possible binary relations of the
elements in LIST = (a b c ...). Iff WITH-IDENTITY is non-nil the
identity relations (a a), (b b), (c c) is included in the result.
If ORDERED is non-nil, both (a b) and its reflection, (b a), is
included in the result. Else, only the element (a b) is included,
where a comes before b in LIST."
  (loop for sublist1 on list nconc
	(loop for sublist2 on (if with-identity sublist1 (rest sublist1))
	      collect (list (first sublist1) (first sublist2))
	      if (and ordered
		      (neq (first sublist1) (first sublist2)))
	      collect (list (first sublist2) (first sublist1)))))
;;(relations '(a b c))

(defun incf-list (list ranges)
  "Not exported."
  (when (listp list)
    (incf-list (rest list) (rest ranges))
    (when (zerop (second list))
      (incf (first list)))
    (when (= (first list) (first ranges))
      (setf (first list) 0))
    list))
;;(incf-list '(0 0) '(2 3))

(defun sequence-index-boundary (sequence)
  "Not sure how useful this is in general."
  (list 0 (1- (length sequence))))

;;; TODO: tree utils again: consider writing tree-utils and move them there
(defun tree->value-index-tuples (tree &optional indexes)
  "Converts an N-dimensional list TREE to a list of array entries.
Each array entry is of the form (VALUE INDEX-TUPLE), where VALUE
denotes the values to be associated with INDEX-TUPLE."
  (if (atom tree)
    (list (list tree (reverse indexes)))
    (loop for i from 0
	for x in tree
	append (tree->value-index-tuples x (cons i indexes)))))
;;(tree->value-index-tuples qwe)
;;(setf qwe '((a b) (a b)))

(defun tree-dimensions (tree)
  "Returns the dimensions of TREE when regarded as an N dimensional
matrix, where N is the depth of TREE. This version does not consider
sparse trees, but assumes the the number of elements on a tree level
is the same throughout TREE."
  (loop for x = tree then (car x) 
	while (consp x) collect (length x)))
;;(tree-dimensions qwe)

(defun tree->array (tree &key (key #'identity))
  (let ((array (make-array (tree-dimensions tree))))
    (loop for (v i) in (tree->value-index-tuples tree)
	  do (setf (apply #'aref array i) (funcall key v)))
    array))
;;(tree->array qwe)

(defun parse-windows-path (path) (split-by-char path #\\ t))
(defun windows-path->unix-path (path)
  (format nil "~{~a~^/~}" (parse-windows-path path)))
;;(windows-path->unix-path (ext:getenv "HOMEPATH"))

(defun win32-homepath ()
  (format nil "/cygdrive/c/~a/"
	  (windows-path->unix-path
	   #+clisp
	   (ext:getenv "HOMEPATH")
	   #+sbcl
	   (sb-ext:posix-getenv "HOME")
	   #-(or clisp sbcl)
	   (error "Not implemented"))))
;;(merge-pathnames "Google Drive/Contango-MB/Light Structures" (win32-homepath))

(defun list< (list1 list2 &optional (lt #'<))
  (loop for x1 in list1 for x2 in list2
	when (funcall lt x1 x2) return t
	never (funcall lt x2 x1)
	finally (return nil)))
;;(list< '(0 0) '(0 1))

(defun parse-iso-date (iso-date) (mapcar #'parse-integer (split-by-char iso-date #\-)))
;;(parse-iso-date "2015-02-20")
(defun parse-iso-time (iso-time) (mapcar #'parse-integer (split-by-char iso-time #\:)))
;;(parse-iso-time "11:55")
(defun parse-iso-dttm (iso-dttm)
  (destructuring-bind (iso-date iso-time) (split-by-char iso-dttm #\T)
    (nconc (parse-iso-date iso-date)
	   (parse-iso-time iso-time))))
;;(parse-iso-dttm "2003-04-05T11:48")

(defun iso-time (&key (universal-time (get-universal-time)) (format :full-iso))
  (multiple-value-bind (s mi h d mo y dlp z)
      (decode-universal-time universal-time)
    (declare (ignore dlp z))
    (if (stringp format)
      (format nil format y mo d h mi s)
      (case format
	(:iso-dttm (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" y mo d h mi s))
	(:iso-dttm-sans-T (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y mo d h mi s))
	(:iso-dttm-file (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d-~2,'0d-~2,'0d" y mo d h mi s))
	(:iso-date (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d))
	(:iso-time (format nil "~2,'0d:~2,'0d:~2,'0d" h mi s))
	(:iso-time-sans-seconds (format nil "~2,'0d:~2,'0d" h mi))
	(t (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" y mo d h mi s))))))
;;(iso-time :format "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d Z")
;;(iso-time)

;;;; array utils
(defun unit-sequence (dimension length &key (unit 1) (zero 0) (type 'list))
  "Returns a Euclidian unit vector of LENGTH for DIMENSION as a list."
  (let ((res (make-list length :initial-element zero)))
    (setf (nth dimension res) unit)
    (if (eql type 'list) res (coerce res type))))
;;(unit-sequence 1 3 :unit 2 :zero 'zzzero :type 'vector)
;;(unit-sequence 1 4 :type 'vector)

(defmethod row-major-index->index ((dimensions cons) row-major-index)
  (loop for d in (butlast dimensions)
	for n = row-major-index then remainder
	for (index remainder) = (multiple-value-list (floor n d))
	collect index into res
	finally (return (rcons res remainder))))
;;(loop for i below 6 collect (row-major-index->index '(2 3) i))

(defmethod row-major-index->index ((x array) row-major-index)
  (row-major-index->index (array-dimensions x) row-major-index))
;;(loop for i below 6 collect (row-major-index->index (make-array '(2 3)) i))

(defun array-row (array &optional (i 0))
  (make-array (rest (array-dimensions array))
    :displaced-to array
    :displaced-index-offset (apply #'array-row-major-index array (unit-sequence 0 (array-rank array) :unit i))))
;;(array-row #3A(((1 2) (3 4)) ((1 2) (3 4))) 1)

(defun array-rows (array)
  (loop for i below (array-dimension array 0)
	collect (array-row array i)))
;;(setf ewq #2A((1 2) (3 4))) (array-rows ewq)

(defun array-column (array &optional (j 0) (from 0) (to (array-dimension array 0)))
  "Returns column J in ARRAY from row FROM to row TO as a vector"
  (coerce (loop for i from from below to collect (aref array i j)) 'vector))
;;(array-column #2A((1 2) (3 4) (5 6)) 0 1 2)

(defun generate-array (dimensions &optional (fn (lambda (i) i)))
  "Generates a new array of DIMENSIONS with unary function FN.
FN takes a positive integer as argument, and returns the value of the
corresponding row major index in the new array."
  (let ((res (make-array dimensions)))
      (loop for i below (array-total-size res)
	    do (setf (row-major-aref res i) (funcall fn i)))
      res))
;;(generate-array '(2 2)) ==> #2A((0 1) (2 3))

(defmethod dimensions ((x cons)) x)
(defmethod dimensions ((x array)) (array-dimensions x))
;;(mapcar #'dimensions '((2 2) #2A((1 2) (3 4))))

(defun random-interval (a &optional b)
  "Returns a number randomly distributed in the interval [A B[."
  (if b
    (+ a (random (- b a)))
    (if (plusp a) (random a) (random-interval a 0))))
;;(random-interval -10.0)

(defun random-array (dimensions-designator &optional (interval '(0 1.0)))
  "Returns an array of DIMENSIONS-DESIGNATOR where each array element is randomly distributed number in INTERVAL.
DIMENSIONS-DESIGNATOR is either a cons or an array.
If INTERVAL is a number, then the interval [0 INTERVAL[ is assumed"
  (generate-array (dimensions dimensions-designator) (lambda (i)
						       (declare (ignore i))
						       (apply #'random-interval (listify interval)))))
;;(random-array '(2 2) (* 2 pi))
;;(random-array #2A((0.9952052 0.81327087) (0.98107666 0.29935586)))

(defun subscripts-rev (dimensions row-major-index)
  "Helper function for SUBSCRIPTS"
  (loop for n = row-major-index then (/ (- n m) d)
	for d in (reverse dimensions)
        for m = (mod n d)
	collect m))

(defun subscripts (dimensions row-major-index)
  "Converts ROW-MAJOR-INDEX corresponding to DIMENSIONS to subscripts."
  (nreverse (subscripts-rev dimensions row-major-index)))
;;(loop for i below 6 collect (subscripts '(3 2) i))

(defun array-subscripts (array row-major-index)
  "Converts ROW-MAJOR-INDEX corresponding to ARRAY's dimensions to subscripts.
That is x == (array-row-major-index arr (array-subscripts arr x))."
   (subscripts (array-dimensions array) row-major-index))

(defmethod span-array (fn &rest sequences)
  "Returns an array A, so that the element at subscript (I1 I2 ...) equals (FN S1(I1) S2(I2) ...),
where S1, S2, ... are the elements in SEQUENCES."
  (let ((dims (mapcar #'length sequences)))
    (flet ((span (i) (apply fn (mapcar #'elt sequences (subscripts dims i)))))
      (generate-array dims #'span))))
;;(span-array #'* #(1 2) #(1 2 3))

(defun map-array (fn &rest arrays)
  "Returns an array A of the same dimensions as arrays, and where A(I)
is (fn A1(I) A2(I) ...), I being a row major index."
  (generate-array (and arrays (array-dimensions (first arrays)))
		  (lambda (i) (apply fn (mapcar (bind #'row-major-aref i) arrays)))))
;;(map-array #'+ #3A(((101 2) (3 4)) ((1 2) (3 4))) #3A(((1 2) (3 4)) ((1 2) (3 4))))
;;(map-array (bind #'* 2) #3A(((101 2) (3 4)) ((1 2) (3 4))))

(defun map-array-rows (fn 2a &optional (type 'vector))
  (coerce (mapcar fn (array-rows 2a)) type))
;;(map-array-rows #'length (tree->array '((1 2) (3 4))))

(defun array->tree (array)
  "This works only for 1d and 2d arrays"
  (case (array-rank array)
    (0 (aref array))
    (1 (coerce array 'list))
    (2 (loop for row in (array-rows array) collect (coerce row 'list)))
    (t (error "ARRAY->TREE is not implemented for arrays of rank ~a" (array-rank array)))))
;;(mapcar #'array->tree (list #0A1234 #1A(1 2 3 4) #(1 2 3 4) #2A((1 2) (3 4))))

(defun array-reverse-rows (a)
  "Reverts rows in A. ``Slower'' version."
  (tree->array (nreverse (array->tree a))))
;;(array-reverse-rows (tree->array '((a b) (c d))))
;;(time (progn (array-reverse-rows (make-array '(1000 1000))) 'ok))
;;(trace array-reverse-rows)

(defun array-nreverse-rows (a)
  "Destructive ``faster'', but in fact, no big gain."
  (destructuring-bind (n m) (array-dimensions a)
      (loop for il below (floor n 2)
	    for iu downfrom (1- n)
	    do (loop for j below m do (rotatef (aref a il j) (aref a iu j)))))
  a)
;;(array-nreverse-rows (tree->array '((a b) (c d) (e f))))
;;(time (progn (array-nreverse-rows (make-array '(1000 1000))) 'ok))

(defun swap-rows (a i1 i2 &key (from 0) to)
  "Swaps rows I1 and I2 in two-dimensional array A. Destructive."
  (when (/= i1 i2)
    (loop for j from from below (or to (array-dimension a 1))
	  do (rotatef (aref a i1 j) (aref a i2 j))))
  a)
;;(let ((a #2a((1 2 3) (2 3 4) (4 5 6)))) (list (swap-rows a 0 1) a))

(defmacro alias (new-name prev-name)
  "From On Lisp"
  `(defmacro ,new-name (&rest args)
     `(,',prev-name ,@args)))

(defun nboundaries-1 (list)
  (loop for (a b) in (pairs list) collect (/ (+ b a) 2)))
;;(boundaries-1 '(1.0 3.0 4.0))

(defun nboundaries (list &optional flank-p)
  (nboundaries-1 (if flank-p
		  (nflank (- (* 2 (car list)) (cadr list))
			  list
			  (- (* 2 (car (last list))) (car (last list 2))))
		  list)))

(defun boundaries (list &optional flank-p)
  (nboundaries (copy-seq list) flank-p))
;;(let ((l (list -4.0 -3.0 -1.0))) (list (boundaries l t) l))

;;;; Not exported:
;;; These utils may seem a bit obscure, but they are pretty fast on lists.
(defun dist-rev-list (list)
  "Returns the distances between the numbers in the reversion of LIST.
It is meant as a helper function for DELTAS-LIST."
  (let ((res))
    (do ((l list (setf l (cdr l))))
	((null (cdr l)) res)
      (push (- (cadr l) (car l)) res))))
;;(dist-rev-list (a-b 1 4))

(defun deltas-list (list)
  "Fast implementation of DELTAS for list sequence. See DELTAS."
  (let* ((list (dist-rev-list list))
	 (res (list (car list))))
    (do* ((l list (setf l (cdr l))))
	 ((null (cdr l)) (cons (car l) res))
      (push (/ (+ (cadr l) (car l)) 2.0) res))))
;;(deltas-list '(0 1 3))

;;; Here is the sequence version (not so fast)
(defun deltas (sequence)
  "Returns the canonical deltas around numbers in sequence.
If A, B, C are consecutive numbers, the delta for B is (- (/ (+ A C) 2) B)."
  (coerce (deltas-list (coerce sequence 'list)) (type-of sequence)))
;;(deltas #(0 1 3))

(defmacro with-transpose ((var tree) &body body)
  "Executes BODY with the transpose of TREE bound to VAR returning the transpose of the result of BODY."
  `(let ((,var (transpose-tree ,tree)))
     (transpose-tree (progn ,@body))))
;;(with-transpose (it '((1 2 3) (a b c))) (sort it #'> :key #'first))

(defun expand-list (list &optional (n 1) wrap)
  "Expands LIST with N elements in at both ends. If WRAP is nil, LIST
is expanded at the frond and the end by increments defined as the
numerical difference between the two first and two last elements in
LIST, respectively. If WRAP is a number, LIST is expanding by cycling
on a ring of length WRAP."
  (flet ((wrap (list x) (mapcar (bind #'+ x) list)))
    (if wrap
      ;;cyclic extension
      (if (numberp wrap)
	(append (wrap (last list n) (- wrap)) list (wrap (head list n) wrap))
	(append (last list n) list (head list n)))
      ;;linear extension
      (let ((a (first list))
	    (b (last-elt list)))
	(let ((deltas (if (> (length list) 1)
			(list (- (second list) a) (- b (last-elt list 2)))
			(list 1 1))))
	  (destructuring-bind (df de) deltas
	    (append (loop for i below n for x = (- a df) then (- x df) collect x)
		    list
		    (loop for i below n for x = (+ b de) then (+ x de) collect x))))))))
;;(expand-list '(1 2 3 5) 2)
;;(expand-list '(1 2 3) 2 5)
;;(expand-list '(1 1 1))

(defun expand-tree (tree dimension &optional (n 1) wrap)
  "EXPANDS tree just like EXPAND-LIST in the specified DIMENSION.
With DIMENSION set to 0 it is equivalent to EXPAND-LIST."
  (case dimension
    (0 (with-transpose (x tree) (expand-tree x 1 n wrap)))
    (1 (loop for row in tree collect (expand-list row n wrap)))
    (t (error "EXPAND-TREE does not support DIMENSION > 1"))))
;;(expand-tree '((1 2 3) (11 12 13)) 0 2)

(defun expand-sequence (sequence &optional (n 1) wrap)
  "Generalization of EXPAND-LIST to sequences."
  (with-tree (x sequence) (expand-list x n wrap)))
;;(let ((v #(1 2 3 4))) (list (expand-sequence v) v))

;;; this one is currently not exported
(defmacro with-list ((var sequence &optional res-type) &body body)
  (with-gensyms (gsequence)
    `(let ((,gsequence ,sequence))
       (let ((,var (coerce ,gsequence 'list)))
	 (coerce (progn ,@body) (or ,res-type (class-of ,gsequence)))))))
;;(with-list (x '(1 2 3) 'vector) (rest x))

(defun nthcdr* (list &optional (n 1))
  "Same as NTH, but accepts negative arguments. If N < 0 then (nth* N
  LIST) returns the Nth last element in LIST. In fact, if L is the
  length of LIST, it returns always the Mth element in LIST, where 0
  <= M < L and M is equal to N modulo L."
  (nthcdr (mod n (length list)) list))

(defun nrotate-list (list &optional (n 1))
  (nconc (nthcdr* list n)
	 (and (plusp (mod n (length list)))
	      (butlast* list (- n)))))
;;(setq l '(a b c d e))
;;(nrotate-list l 7)

(defun rotate-list (list &optional (n 1))
  "Returns a list that is LIST rotated N times."
  (nrotate-list (copy-list list) n))

(defun rotations (list)
  (loop for i below (length list)
	collect (rotate-list list i)))
;;(rotations '(a b c))
;;(rotations '(a))

(defun permutations (list)
  (when list
    (if (rest list)
      (loop for l in (rotations list)
	    append (loop for subperm in (permutations (rest l))
			  collect (cons (first l) subperm)))
      (list list))))
;;(permutations '(a b c))

(defun perm (&rest args) (permutations args))
;;(perm 1 2 3)

(defun nreplace-nth (n new-value sequence)
  "Destructive"
  (setf (elt sequence n) new-value)
  sequence)
;;(let ((s '(0 1 2))) (list (nreplace-nth 1 'qwe s) s))

(defun replace-nth (n new-value sequence)
  (nreplace-nth n new-value (copy-seq sequence)))
;;(let ((s '(0 1 2))) (list (replace-nth 1 'qwe s) s))

;;; external stuff
(defun run-program (program &rest args)
  #+clisp
  (apply #'ext:execute program args)
  #+sbcl
  (apply #'sb-ext:run-program program args)
  #-(or clisp sbcl)
  (error "Not implemented"))
