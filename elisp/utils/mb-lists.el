;;; Generators
(cl-defun a-b (a &optional b inc) (number-sequence a b inc))
(cl-defun 0-n (n &optional inc) (a-b 0 (1- n) inc))
(cl-defun 1-n (n &optional inc) (a-b 1 n inc))
(cl-defun b-a (b &optional a inc) (nreverse (a-b (or a b) b inc)))
(cl-defun n-0 (n &optional inc) (nreverse (0-n n inc)))
;;(1-n 10 2) ==> (1 2 3 4 5 6 7 8 9 10)
;;(a-b 0 10)
;;(b-a 10 8 2)

(cl-defun transpose (lists)
  (when lists (apply #'cl-mapcar #'list lists)))

(cl-defun nrcons (list x)
  (nconc list (list x)))
;;(nrcons '() 1)

(cl-defun rcons (list x)
  (append list (list x)))
;;(rcons '() 1)

(cl-defun nth* (n list)
  "Same as NTH but accepts negative indexes: -1 means last
element, -2 second last and so on."
  (nth (mod n (length list)) list))
;;(cl-mapcar (bind #'nth* (0-n 4)) (a-b -5 5))

(cl-defun head (n list)
  (butlast list (- (length list) n)))
;;(head 2 '(a b c))

(cl-defun l-explicit*< (x y list &rest cl-keys)
  "Returns a binary predicate (less-than-type) that evalutes to t
iff first argument comes before second argument in LIST. If not both arguments are in list, 'NA is returned. Else, it returns nil"
  (let* ((fkey (or (popf cl-keys :fkey) #'identity))
	 (x-pos (apply #'position (funcall fkey x) list cl-keys))
	 (y-pos (apply #'position (funcall fkey y) list cl-keys)))
    (if (and x-pos y-pos)
      (< x-pos y-pos)
      'na)))
;;(l-explicit*< '(c) '(d) '(c a b c d) :fkey #'first)

(cl-defun l-explicit< (x y list &rest cl-keys)
  "Same as l-explicit*<, but returns nil instead of 'na"
  (awhen (apply #'l-explicit*< x y list cl-keys)
    (neq it 'na)))
;;(l-explicit< 'c 'e '(c a b c d))

(cl-defun explicit*< (list &rest cl-keys)
  "Returns a binary predicate similar to `l-explicit*<', but binding the list argument.
TODO: when bind* is finished this method is obsolete (or becomes a simple one-liner):
\\(bind* #'l-explicit*< list 2)"
  (let ((list* list)
		(cl-keys* cl-keys))
    (lambda (x y)
      (l-explicit*< x y list* cl-keys*))))
;;(funcall (explicit*< '(c a b c d)) 'c 'e)

(cl-defun explicit< (list &rest cl-keys)
  "Is to `l-explicit<' what `explicit*<' is to `l-explicit*<'."
  (let ((list* list)
		(cl-keys* cl-keys))
    (lambda (x y)
      (let ((x-pos (apply #'position x list* cl-keys*))
	    (y-pos (apply #'position y list* cl-keys*)))
	(and x-pos y-pos (< x-pos y-pos))))))
;;(funcall (explicit< '(c a b c d)) 'e 'c)

(cl-defun group-positions (list &key (test #'eql) (key #'identity))
  "Groups LIST into a list of sublists where all elements are equal
according to TEST and KEY."
  (cl-loop for p in (pairs list :key key)	
	for i from 0
	if (not (apply test p)) collect (1+ i)))
;;(group-positions '(a a b a b) :test #'equal)
;;(group-positions '((1 2) (1 2)) :test #'equal)
;;(equal '(1 2) '(1 2))

;; TODO: rename to split-list-if2; the '2' indicates that splitting
;; is controlled by a /binary/ predicate
;; Also generalize to split-if2
(cl-defun group (list &key (test #'eql) (key #'identity))
  "Groups LIST into a list of sublists where all elements are equal
according to TEST and KEY.
Note that `group' to not consider LIST as a set. To do this, LIST must be sorted first."
  (when list
    (cl-loop for a = 0 then b
	  for b in (group-positions list :test test :key key)
	  collect (subseq list a b) into res
	  finally return (nconc res (list (subseq list a))))))
;;(group '(a a b a b) :test #'equal)
;;(group (cl-sort '(a a b a b) #'string< :key #'symbol-name) :test #'equal)
;;(group '((1 2) (1 3) (1 2)) :test #'equal)
;;(group '(nil nil nil) :test #'equal)
;;(group nil)

(cl-defun equivalence-class-ht (list &key (key #'identity) (test #'eql) (size 65))
  "Helper function for `equivalence-class
Return the hash table"
  (let ((ht (make-hash-table :test test :size size)))
    (cl-loop for x in list
	  for k = (funcall key x)
	  do (puthash k (cons x (gethash k ht)) ht))
    ht))
;;(equivalence-class-ht '((a) (b) (c) (a) (b) (a) (d)) :test #'equal :key #'car)

;; TODO: when the above renaming is done, rename this to GROUP-LIST or
;; perhaps EQUIVALENCE-CLASS-LIST. Or: generalize with ELT, and rename
;; to GROUP or EQUIVALENCE-CLASS.
(cl-defun equivalence-class (list &key (key #'identity) (test #'eql) (size 65))
  "Group LIST into sublist equivalence classes defined by KEY."
  (cl-loop for v the hash-values of (equivalence-class-ht
				  list :key key :test test :size size)
	collect v))
;;(equivalence-class '((a) (b) (c) (a) (b) (a) (d)) :key #'car)

(cl-defun equivalence-class-with-key (list &key (key #'identity) (test #'eql)
					     (size 65))
  "Group LIST into sublists (K ELEMENTS) where for each group
ELEMENTS are exactly the elements in LIST that evaluates to K."
  (cl-loop for v the hash-values of (equivalence-class-ht
				  list :key key :test test :size size)
	using (hash-keys k)
	collect (list k v)))
;;(equivalence-class-with-key '((a) (b) (c) (a) (b) (a) (d)) :key #'car)

(cl-defun listify-atoms (list-or-atoms)
  (let* ((list (find-if #'listp list-or-atoms))
	 (n (length list)))
    (cl-loop for x in list-or-atoms collect (if (listp x) x (make-list n x)))))
;;(listify-atoms '(a (1 2 3) (d f)))

(cl-defun nzip (&rest lists) 
  (apply #'nconc (transpose lists)))

(cl-defun zip (&rest lists) 
  (apply #'nzip (listify-atoms (copy-tree lists))))
;;(zip '(0 2 4) 1)
;;(butlast (zip '(0 2 4) '(1 3 3)))

(cl-defun nunzip (list &optional (n 2))
  "Destructive version of `ZIP'
TODO: something is wrong, see test below."
  (cl-loop with heads = (cl-loop for i below n collect (nthcdr i list))
	with pointers = (cl-copy-list heads)
	while (first pointers)
	do (cl-loop for p in-ref pointers 
		 while p
		 do (progn (setf (cdr p) (nthcdr n p))
			   (setf p (cdr p))))
	finally return heads))
;;(nunzip (0-n 10) 2)

(cl-defun unzip (list &optional (n 2)) (nunzip (cl-copy-list list) n))
;;(unzip (0-n 10) 3)

(cl-defun repeat-elements (x &optional n)
  (awhen (make-list (or n 2) x)
    (apply #'zip it)))
;;(repeat-elements (0-n 3) 2)

(require 'function-signature)
(cl-defun infix-list (list infix &optional infix-is-function-p) 
  "Zip list with INFIX-es.
'(a b c) => '(a INFIX b INFIX c).

INFIX may be a function with signature (I &optional N), where
argument I is the nth time FN will be called by INFIX-LIST, and N
is the total number of calls. If you want the function to
actually call INFIX you must set the optional argument
INFIX-IS-FUNCTION-P to NON-NIL. Otherwise the function will treat
INFIX as a constant sexp."
  (when list
    (let ((n (length list)))
      (butlast (zip list (if infix-is-function-p
			   (mapcar (if (= 1 (cdr (function-arity infix)))
				     infix
				     (bind infix n))
			     (0-n n))
			   (make-list n infix)))))))
;(infix-list '(a b c) #'1+ t)

(cl-defun nflank (a list &optional (b a))
  "Inserts A before and B after LIST. Destructive."
  (nconc (cons a list) (list b)))
;;(let ((l (list 2 3))) (list l (nflank 1 l 4)))

(cl-defun flank (a list &optional (b a))
  "Inserts A before and B after LIST."
  (nflank a (cl-copy-list list) b))
;;(flank 1 '(a))

(defmacro twins (x) `(make-list 2 ,x))
;;(twins (+ 1 2))

(cl-defun pairs (list &key (key #'identity) (flank-p nil))
  "Returns the elements in list as consecutive pairs.
I.e. for list \(x11 x2 x3 ... xn-1 xn\), it returns the list
\((x1 x2) (x2 x3) ... (xn-1 xn)\) See also `tuples' for a more
generalized version. If flank-p is non-nil the result is
`flank'ed with pairs of its first and last element."
  (let ((res (cl-loop for (x y) on (mapcar key list) while y collect (list x y))))
    (if flank-p
      (nflank (twins (caar res)) res (twins (cadar (last res))))
      res)))
;;(pairs '(1 2 3 4 5) :key #'1+ :flank-p t)

(cl-defun tuples (list n)
  "Returns the elements in list as consecutive tuples.
I.e. for list \(x11 x2 x3 x4 ... xn-2 xn-1 xn\), with N = 3, 
it returns the list \((x1 x2 x3) (x2 x3 x4) ... (xn-2 xn-1 xn)\)
TODO: implement a mapping key, see `pairs' (when needed)"
  (cl-loop for h on list
	for i from (- (length list) n) downto 0
	collect (butlast h i)))
;;(tuples '(a b c d e) 3)

(cl-defun power-set-indices (n)
  "Return the indices corresponding to a power set for at set with N elements"
  (if (zerop n)
    '(())
    (let ((psi-1 (power-set-indices (1- n))))
      (append psi-1 (mapcar (bind #'rcons (1- n)) psi-1)))))

(cl-defun power-set-indices (n)
  "Return the indices corresponding to a power set for at set with N elements"
  (if (zerop n)
    '(())
    (let ((psi-1 (power-set-indices (1- n))))
      (append psi-1 (mapcar (lambda (x) (cons (1- n) x)) psi-1)))))
;;(power-set-indices 3)
;;(length (power-set-indices 4))

(cl-defun power-set (list)
  "Return the set of all subsets of list taken as a set."
  (maptree (bind #'nth list) (power-set-indices (length list))))
;;(power-set '(a b c))

(cl-defun cut (list &optional (n 2) (include-remainer nil))
  "Cuts LIST into sublists of length N while preserving order.
If INCLUDE-REMAINER is nil and last element in the result list
has length shorter than N, this last element is discarded."
  (cl-assert (plusp n)) ;; otherwise we'll fall into an infinite loop
  (cl-loop for x on list by (bind #'nthcdr n 1)
	collect (butlast* x (- n)) into res
	finally return (if (nor include-remainer
				(zerop (mod (length list) n)))
			 (butlast res) res)))
;;(cut (0-n 5) 3 nil)
;;(cut '(1 2 3 4 5) 2 t)

(cl-defun cut-list-if (predicate list inclusion &rest args)
  "Cut LIST in sublists where PREDICATE is true.
If INCLUSION is not nil, then the element in LIST matching
PREDICATE is included in the result."
  (cl-loop for start = 0 then end
	for end in (apply #'positions-if predicate list args)
	if (subseq list start end) collect it into res
	finally (return (if inclusion
			  (append res (list (subseq list (or end 0))))
			  (mapcar #'(lambda (x) (subseq x 1))
			    (append res (list (subseq list (or end 0)))))))))
;;(cut-list-if #'primep (0-n 10) t)
(cl-indent 'cut-list-if 'prog1)

(cl-defun relations (list &optional with-identity ordered)
  "Returns a list containing all possible binary relations of the
elements in LIST = (a b c ...). Iff WITH-IDENTITY is non-nil the
identity relations (a a), (b b), (c c) is included in the result.
If ORDERED is non-nil, both (a b) and its reflection, (b a), is
included in the result. Else, only the element (a b) is included,
where a comes before b in LIST."
  (cl-loop for sublist1 on list nconc
	(cl-loop for sublist2 on (if with-identity sublist1 (rest sublist1))
	      collect (list (first sublist1) (first sublist2))
	      if (and ordered
		      (neq (first sublist1) (first sublist2)))
	      collect (list (first sublist2) (first sublist1)))))
;;(relations '(a b c))

(cl-defun combine2 (list1 list2 &optional ordered)
  (if ordered
    (nconc (combine2 list1 list2)
	   (combine2 list2 list1))
    (cl-loop for x1 in (listify list1) nconc 
	  (cl-loop for x2 in (listify list2)
		collect (list x1 x2)))))
;;(combine2 '(a) '())
;;(combine2 '(a) '(b))
;;(combine2 '(a b) '(c d e) t)
;;(combine2 '(a b) '(c d e) t)
;;(combine2 '(a (b c)) '(c d) t)
;;(combine2 '(a b) nil t)

(cl-defun combine-1 (lists)
  "Generalization of combine2"
  (cl-case (length lists)
    (0 nil)
    (1 (list (listify (first lists))))
    (2 (combine2 (first lists) (second lists)))
    (t  (cl-loop for (x y) in (combine2 (first lists)
				  (combine-1 (rest lists)))
	   collect (cons x y)))))
;;(combine-1 '((a b) c (d e)))

(cl-defun combine (lists &key key)
  "Generalization of combine2"
  (if key
    (mapcar (bind #'apply key 1) (combine-1 lists))
    (combine-1 lists)))
;;(combine '((0 1 2) (("3" 1) ("7" 1))))

(cl-defun test-arguments (fn args)
  (combine args :key #'(lambda (&rest args)
			 (format "%S: %S" args (apply fn args)))))
;;(test-arguments #'concat '(("a" "b") ("c") ("e" "f")))

(cl-defun accumulate-sorted-list (list &key (test #'eql)
					 (min-occurrences 1)
					 (key #'identity)
					 (accumulator #'length))
  "Accumulates sorted LIST. See `accumulate-list' for details."
  (cl-loop for x in (group list :test test :key key)
	for l = (funcall accumulator x)
	if (>= l min-occurrences) collect (list (funcall key (first x)) l)))
;;(accumulate-sorted-list '((a 1) (a 2) (b 4) (b 5) (c 100)) :key #'first :accumulator #'(lambda (x) (sum x :key #'second)))
;;(accumulate-sorted-list '(a a a b b c d c) :test 'equal :min-occurences 2)

(cl-defun accumulate-list (list &key (test #'<) (min-occurrences 1)
				  (key #'identity) (accumulator #'length))
  "Returns a list of pairs (X count-X), where X is an element in
list and count-X is the number of occurrences of X. Note that the
resulting list is sorted on the value of COUNT-X"
  (accumulate-sorted-list (cl-sort (cl-copy-list list) test :key key)
    :test (lt-equal test) :key key
    :min-occurrences min-occurrences :accumulator accumulator))
;;(accumulate-list '((a 1) (a 2) (b 4) (c 100) (b 5)) :test #'symbol< :key #'first :accumulator #'(lambda (x) (sum x :key #'second)))
;;(accumulate-list '(a b c a b a d) :test #'symbol<)
;;(accumulate-list '(a b c a b a d) :test #'symbol< :min-occurences 2)

(cl-defun repetitions-1 (list &optional (start-index 0) (test #'eql))
  "Returns a list of repetitions from first element in list"
  (cl-loop with target-list = list
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
;;(repetitions-1 '(a b c a b c a b c a b c))
;;(repetitions-1 '(a))

(cl-defun repetitions (list &optional (test #'eql))
  "Returns a description of all repetitive patterns in LIST.
Each element is on the form ((INDEX LENGTH) COUNT), where INDEX
is the start position of pattern, LENGTH is the pattern length
and COUNT is the number of repetitions of the pattern. Note that
COUNT really counts _repetitions_, so a value of 1 means two
sucsessive occurences of the pattern.
Algorithm i O(n^2)."
  (cl-loop for x on list 
	for i from 0
	if (repetitions-1 x i test)
	append (accumulate-sorted-list it #'equal)))
;;(repetitions '(a a a a a a)) => (((0 1) 5) ((1 1) 4) ((2 1) 3) ((3 1) 2) ((4 1) 1))
;;(repetitions '(a b a b a))
;;(cl-loop for i below 10 do (repetitions '(a b a b a)))

(cl-defun x-repetitions (list)
  "A Q&D version of `repetitions'. Its algorith is far simpler in
code, but is much slower (O(n^3)). Also, the result is not so
informative."
  (cl-loop for i from 1 to (floor (length list) 2)
	append (cl-loop for k below i
		     for shifted-list on list
		     for i-tuples = (cut shifted-list i)
		     append (accumulate-sorted-list i-tuples #'equal 2))))
;;(x-repetitions '(a b a b a b))
;;(x-repetitions (0-n 500))
;;(repetitions (0-n 500))

;;; List functions
(cl-defun swap-head (list predicate)
  "Swaps first element in LIST with the first element in that
matches PREDICATE"
  (awhen (member-if predicate list)
    (cl-rotatef (first it) (first list))
    list))
;;(dv-swap-head '(4 3 2 1) (bind #'< 4))

(defmacro mrotate-list (list &rest positions)
  "Swaps Ith and Jth elements in LIST"
  (let ((glist (gensym)))
    `(let ((,glist ,list))
       (cl-rotatef ,@(mapcar #'(lambda (x) `(nth ,x ,glist)) positions))
       ,glist)))
;;(mrotate-list '(a b c d) 0 2 3)

(cl-defun nrotate-list (list &optional (n 1))
  (when list
    (let ((new-list (nthcdr (mod n (length list)) list)))
      (setf (nthcdr (mod n (length list)) list) nil)
      (setf (cdr (last new-list)) list)
      new-list)))
;;(setq l '(a b c d e))
;;(nrotate-list l)

(cl-defun rotate-list (list &optional (n 1))
  "Returns a list that is LIST rotated N times."
  (nrotate-list (cl-copy-list list) n))

(cl-defmacro rotatef-list (list &optional (n 1))
  `(setf ,list (rotate-list ,list ,n)))

(cl-defun rotate (sequence &optional (n 1))
  "Returns a list that is LIST rotated N times."
  (nrotate-list (cl-copy-list list) n))

(defmacro list-insert (x n list)
  "Inserts element X at position N in LIST. Returns the tail of
LIST with X as head"
  `(push ,x (nthcdr ,n ,list)))
;;(let ((qwe (transpose '((a b) (a b) (a b))))) (list (list-insert '(1 2 3) 1 qwe) qwe))

(cl-defun list-substitute (x n list &optional (length 1))
  "Substitues element at position N LIST with X
With optional LENGTH, it substitutes more than one element with X"
  (setf (nthcdr n list) (cons x (nthcdr (+ n length) list))))

(cl-defun list-substitute-list (newlist n list &optional (length 1))
  "Substitutes sublist starting at position N in LIST with NEWLIST.
Optional LENGTH defines length of substituted sublist."
  (setf (nthcdr n list) (nconc newlist (nthcdr (+ n length) list))))
;;(let ((qwe '(a b c d)) (ewq '(x y))) (list-substitute-list '(x y) 1 qwe 1) (list qwe ewq))

(cl-defun butlast* (list &optional n)
  "Same as `butlast' but accepts negative argument, meaning counting from start."
  (butlast list (mod (or n 1) (length list))))
;;(butlast* '(a b c d e) -2)

(cl-defun nsplit-nth (n list)
  "Returns Nth element in LIST and the remainder of LIST. Destructive.
TODO: this looks like draw. Check out and clean up if necessary"
  (if (zerop n)
    (list (car list) (setf list (cdr list)))
    (let* ((prev (nthcdr (1- n) list))
	   (element (cadr prev)))
      (setf (cdr prev) (cddr prev))
      (list element list))))
;;(let ((l (list 0 1 2 3))) (values (nsplit-nth 1 l) l))

(cl-defun test-nsplit-nth (&optional (n 3))
  (let ((list (0-n n)))
    (cl-loop for i below n
	  collect (let ((list* (cl-copy-list list)))
		    (values (nsplit-nth i list*) list*)))))
;;(test-split-nth) => (((0 (1 2)) (0 1 2)) ((1 (0 2)) (0 2)) ((2 (0 1)) (0 1)))

(cl-defun filter-duplicates (list1 list2 &key (test #'eql) (start 0) end)
  "Removes all elements from row and fasit that are equal and is
in the same position."
  (cl-loop for elt1 in (subseq list1 start end)
	for elt2 in (subseq list2 start end)
	unless (funcall test elt1 elt2)
	collect elt1 into list1* and collect elt2 into list2*
	finally return (list list1* list2*)))
;;(filter-duplicates '(1 2 3 4) '(1 4 2 3) :test #'(lambda (x y) (eql (cl-oddp x) (cl-oddp y)))) ;;filters equal parity

(cl-defun list< (list1 list2 &key (test #'<) (key #'identity))
  "Lexical like comparison of LIST1 and LIST2.
For I from zero Ith element of LIST1 is compared with binary
predicate TEST to the Ith element of LIST2. If they are not equal
the function returns imediately. If the lists are equal at all
positions of the shortest list, the shortest list is considered
less than the longer list.

TEST can also be a list of binary predicates which must be at
least as long as the shortest of LIST1 and LIST2. In this case,
the Ith element in TEST is used to compare the Ith elements of
the two lists.

The function also accepts a :key keyword, which is used in the
canonical sense, see for instance `cl-find'

TODO: turn this into sequence<"
  (cl-loop with tests = (if (atom test)
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
;;(list< '("n") '("n" "c1") :test #'string<)
;;(list< '((a a) (a)  (b b)  (a) (b)) '((b b) (a) (b) (a a) (a)) :key #'length)
;;(list< '(1 "b") '(1 "b") :test #'string< :key #'sstring)
;;(list< '("n" "c1" "c3") '("n" "c1" "c2") :test #'string<)

(cl-defun list> (list1 list2 &rest args)
  "See `list<'"
  (apply #'list< list2 list1 args))
;;(list> '(1 2 3 5) '(1 2 3 3 3))

(cl-defun pad-lists (lists &key (elt 0) (from-end t))
  "Returns a copy of LISTS, but with each list padded with ELT so
that all lists have the same length. If FROM-END is nil, each
list is prefixed with ELTs, otherwise it is suffixed."
  (let* ((lengths (mapcar #'length lists)))
    (cl-mapcar #'(lambda (l n)
		   (if from-end 
		     (append l (make-list n elt))
		     (append (make-list n elt) l)))
	       lists (mapcar (bind #'- (minimum lengths #'>) 1) lengths))))
;;(pad-lists '((1) (1 1) (1 1 0)) :elt 'qwe :from-end nil)

(cl-defun vlist< (l1 l2)
  "A simple, but much slower version of `version-list-<'. The
functions is meant as an example of how to use pad-lists. Btw, it
would be interesting to see why the running times are so extremely
different, see test cases below."
  (apply #'list< (pad-lists (list l1 l2))))
;;(time (cl-loop repeat 100000 do (vlist< '(1 2) '(1 1))))
;;(time (cl-loop repeat 100000 do (version-list-< '(1 2) '(1 1))))

(cl-defun ninsert-sorted-list (x list)
  (if list
    (if (< x (car list))
      (cons x list)
      (cl-loop with l = list
	    while (and (cdr l) (> x (cadr l)))
	    do (setf l (cdr l))
	    finally
	    do (setf (cdr l) (cons x (cdr l)))
	    return list))
    (cons x nil)))
;;(let ((l '(0 2))) (ninsert-sorted-list 1 l) l)
;;(let ((l '(0))) (ninsert-sorted-list 1 l) l)
;;(let ((l '())) (ninsert-sorted-list 1 l))

;;;; Sorted trees
(cl-defun insert-sorted-tree (x tree)
  (if tree
    (if (< x (first tree)) ;search left tree
      (if (second tree)
	(insert-sorted-tree x (second tree))
	(setf (second tree) (list x nil nil)))
      (if (third tree)
	(insert-sorted-tree x (third tree))
	(setf (third tree) (list x nil nil))))
    (setf tree (list x nil nil))))

(cl-defun test-insert-sorted-tree (&optional (n 5) (limit 20))
  (let (tree)
    (setf tree (insert-sorted-tree 8 tree))
    (cl-loop for i in (list 14 7 11 5 7 3)
	  do (insert-sorted-tree i tree))
    (remove-minimum-element-sorted-tree tree)
    tree))
;;(test-insert-sorted-tree)

(cl-defun remove-minimum-element-sorted-tree (tree)
  (if (second tree)
    (setf (second tree) (remove-minimum-element-sorted-tree (second tree)))
    (if (third tree)
      (setf (third tree) (remove-minimum-element-sorted-tree (third tree)))
      (setf tree nil)))
  tree)
;;(test-insert-sorted-tree)

(cl-defun collect-sorted-tree (tree &optional (res ()))
  (if tree
    (nconc (collect-sorted-tree (second tree))
	   (list (first tree))
	   (collect-sorted-tree (third tree)))))
;;(collect-sorted-tree (test-insert-sorted-tree))

(cl-defun sort-to-order (list order predicate &optional (key #'identity))
  (let ((order-positions (make-hash-table)))
    (cl-loop for x in order
	  for i from 0
	  do (puthash x i order-positions))
    (cl-sort list predicate
	     :key (lambda (x) (gethash (funcall key x) order-positions)))))
;;(sort-to-order '((0) (1) (2)) '(3 2 1 0) #'< #'first)


(cl-defun mapcol (function colpos table)
  "Returns a copy of table, but where the column at COLPOS (only) is mapped by FUNCTION.
The TABLE must be a tree, i.e. a list of lists."
  (mapcar #'(lambda (x) 
	      (let ((x* (cl-copy-list x)))
		(asetf (nth colpos x*) (funcall function it))
		x*))
	  table))

(cl-defun expand-repeat (list n)
  (cl-destructuring-bind (a b) (cl-floor n (length list))
    (append list (flatten (make-list a list)) (subseq list 0 b))))
;;(expand-repeat '(1 2 3 4) 6)

(cl-defun diffs-list (list)
  (cl-loop for (a b) in (pairs list) collect (- b a)))
;;(diffs-list '(1 2 4))

(cl-defun resize-list (list n &optional (extend-fn #'expand-repeat))
  (let ((length (length list)))
    (if (< length n)
      (funcall extend-fn list (- n length))
      (subseq list 0 n))))
;;(resize-list '(1 2 4) 6)

(cl-defun expand-list (list &optional (n 1) (pattern-length 2))
  (let ((diffs (diffs-list (last list pattern-length))))
    (resize-list diffs n)))
;;(expand-list (list 1 2 4))

(cl-defun deltas-list (floats)
  "Finds the mid-points between FLOATS and returns the interval
consisting of each floats neighbouring mid-points."
  (pairs (cl-loop for (a b) in (pairs floats) collect (/ (- b a) 2.0)) :flank-p t))
;;(deltas-list '(1 3 4))

(cl-defun boundaries-1 (list)
  (cl-loop for (a b) in (pairs list) collect (/ (+ b a) 2)))
;;(boundaries-1 '(1.0 3.0 4.0))

(cl-defun boundaries (list &optional flank-p)
  (boundaries-1 (if flank-p
		  (nflank (- (* 2 (car list)) (cadr list))
			  list
			  (- (* 2 (car (last list))) (car (last list 2))))
		  list)))
;;(boundaries (list -4.0 -3.0 -1.0) t)

(cl-defun randomize-intervals (pairs)
  (cl-loop for (a b) in pairs collect (random-float a b)))
;;(randomize-intervals (pairs (0-n 5)))

(cl-defun randomize-elements (sequence)
  (let ((ivs (pairs (boundaries (cl-coerce sequence 'list) t))))
    (values (cl-coerce (randomize-intervals ivs) (type-of sequence))
	    ivs)))
;;(randomize-elements (vector -4.0 -3.0 -1.0))

(cl-defun group-consequtive-integers (integers &optional sorted-p)
  "Group list of INTEGERS into lists of consequtive integers.
E.g. '(1 2 3 6 7 8 11) ==> '((1 2 3) (6 7 8) (11)).

If SORTED-P is not nil, the function assumes that INTEGERS are
sorted with #'<.

The function assumes that there are no duplicates in INTEGERS."
  (let ((sintegers (if sorted-p
		     integers
		     (cl-sort integers #'<))))
    (group sintegers :test #'(lambda (x y)
			       (or (= (1+ x) y)
				   (= (1- x) y))))))
;;(group-consequtive-integers '(1 2 3 6 7 8 11))

(defmacro memcase (list &rest clauses)
  "Same as case, but select the clause in CLAUSES with (member X LIST),
where X is a `car' of one of the CLAUSES. Currently it uses `eql'
for comparison."
  (let ((g (gensym)))
    `(let ((,g ,list))
       (unless (consp ,g)
	 (error "First argument of MEMCASE must be a list!"))
       (cond ,@(mapcar #'(lambda (clause)
                           (let ((k (car clause)))
                             `(,(cond ((member k '(t otherwise))
                                       t)
                                      ((consp k)
                                       `(cl-intersection ,g ',k :test #'eql))
                                      (t `(member ',k ,g)))
                               (progn ,@(cdr clause)))))
                       clauses)))))

(cl-defun swap (x pairs &key (test #'eql))
  "Find PAIR in PAIRS containing X and return the complementary
element of X in pair.

Keywords supported:  :test
"
  (cl-loop for (a b) in pairs
	if (funcall test x a) return b
	if (funcall test x b) return a))
;;(swap 'a '((a b) (c d)))

(cl-defun position-unique (targets list)
  "Return the TARGETS positions in LIST, without repeating index.
If a target in TARGETS matches an element in list, this position
cannot be used for later matches. For example

\(position-unique '(1 1) '(1 2 3 1)) => (0 3)
\(position-unique '(1 1) '(1 2 3)) => (0 nil)
"
  (cl-loop with maps = (cl-loop for x in (cl-remove-duplicates targets)
			 collect (list x (positions x list)))
	for x in targets
	for map = (cl-find x maps :key #'first)
	collect (pop (second map))))
;;(position-unique '(1 1 3 2 3) '(1 2 3))
;;(position-unique '(1 1) '(1 2 3 1))

(provide 'mb-lists)
