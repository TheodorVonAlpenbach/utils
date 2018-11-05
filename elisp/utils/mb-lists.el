;;; Generators
(defun a-b (a &optional b inc) (number-sequence a b inc))
(defun b-a (b &optional a inc) (nreverse (a-b (or a b) b inc)))
(defun 0-n (n &optional inc) (a-b 0 (1- n) inc))
(defun 1-n (n &optional inc) (a-b 1 n inc))
;;(1-n 10 2) ==> (1 2 3 4 5 6 7 8 9 10)
;;(a-b 0 10)
;;(b-a 10 8 2)

(defun transpose (lists)
  (apply #'mapcar* #'list lists))
;;(transpose '((a b c) (d e f)))

(defun nrcons (list x)
  (nconc list (list x)))
;;(nrcons '() 1)

(defun rcons (list x)
  (append list (list x)))
;;(rcons '() 1)

(defun nth* (n list)
  "Same as NTH but accepts negative indexes: -1 means last
element, -2 second last and so on."
  (nth (mod n (length list)) list))
;;(mapcar* (bind #'nth* (0-n 4)) (a-b -5 5))

(defun head (n list)
  (butlast list (- (length list) n)))
;;(head 0 '(a b c))

(defun l-explicit*< (x y list &rest cl-keys)
  "Returns a binary predicate (less-than-type) that evalutes to t
iff first argument comes before second argument in LIST. If not both arguments are in list, 'NA is returned. Else, it returns nil"
  (let* ((fkey (or (popf cl-keys :fkey) #'identity))
	 (x-pos (apply #'position (funcall fkey x) list cl-keys))
	 (y-pos (apply #'position (funcall fkey y) list cl-keys)))
    (if (and x-pos y-pos)
      (< x-pos y-pos)
      'na)))
;;(l-explicit*< '(c) '(d) '(c a b c d) :fkey #'first)

(defun l-explicit< (x y list &rest cl-keys)
  "Same as l-explicit*<, but returns nil instead of 'na"
  (awhen (apply #'l-explicit*< x y list cl-keys)
    (neq it 'na)))
;;(l-explicit< 'c 'e '(c a b c d))

(defun explicit*< (list &rest cl-keys)
  "Returns a binary predicate similar to `l-explicit*<', but binding the list argument.
TODO: when bind* is finished this method is obsolete (or becomes a simple one-liner):
\\(bind* #'l-explicit*< list 2)"
  (lexical-let ((list* list)
		(cl-keys* cl-keys))
    (lambda (x y)
      (l-explicit*< x y list* cl-keys*))))
;;(funcall (explicit*< '(c a b c d)) 'c 'e)

(defun explicit< (list &rest cl-keys)
  "Is to `l-explicit<' what `explicit*<' is to `l-explicit*<'."
  (lexical-let ((list* list)
		(cl-keys* cl-keys))
    (lambda (x y)
      (let ((x-pos (apply #'position x list* cl-keys*))
	    (y-pos (apply #'position y list* cl-keys*)))
	(and x-pos y-pos (< x-pos y-pos))))))
;;(funcall (explicit< '(c a b c d)) 'e 'c)

(cl-defun group-positions (list &key (test #'eql) (key #'identity))
  "Groups LIST into a list of sublists where all elements are equal
according to TEST and KEY."
  (loop for p in (pairs list :key key)	
	for i from 0
	if (not (apply test p)) collect (1+ i)))
;;(group-positions '(a a b a b) :test #'equal)
;;(group-positions '((1 2) (1 2)) :test #'equal)
;;(equal '(1 2) '(1 2))

(cl-defun group (list &key (test #'eql) (key #'identity))
  "Groups LIST into a list of sublists where all elements are equal
according to TEST and KEY.
Note that group to not consider LIST as a set. To do this, LIST must be sorted first."
  (when list
    (loop for a = 0 then b
	  for b in (group-positions list :test test :key key)
	  collect (subseq list a b) into res
	  finally return (nconc res (list (subseq list a))))))
;;(group '(a a b a b) :test #'equal)
;;(group (cl-sort '(a a b a b) #'string< :key #'symbol-name) :test #'equal)
;;(group '((1 2) (1 3) (1 2)) :test #'equal)
;;(group '(nil nil nil) :test #'equal)
;;(group nil)

(defun nzip (&rest lists) 
  (apply #'nconc (transpose lists)))
(defun zip (&rest lists) 
  (apply #'nzip (copy-tree lists)))
;;(butlast (zip '(0 2 4) '(1 3 3)))

(cl-defun nunzip (list &optional (n 2))
  "Destructive version of `ZIP'
TODO: something is wrong, see test below."
  (loop with heads = (loop for i below n collect (nthcdr i list))
	with pointers = (copy-list heads)
	while (first pointers)
	do (loop for p in-ref pointers 
		 while p
		 do (progn (setf (cdr p) (nthcdr n p))
			   (setf p (cdr p))))
	finally return heads))
;;(nunzip (0-n 10) 2)

(cl-defun unzip (list &optional (n 2)) (nunzip (copy-list list) n))
;;(unzip (0-n 10) 3)

(require 'function-signature)
(defun infix-list (list infix &optional infix-is-function-p) 
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
  (nflank a (copy-list list) b))
;;(flank 1 '(a))

(defmacro twins (x) `(make-list 2 ,x))
;;(twins (+ 1 2))

(cl-defun pairs (list &key (key #'identity) (flank-p nil))
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

(cl-defun cut (list &optional (n 2) (include-remainer nil))
  "Cuts LIST into sublists of length N while preserving order.
If INCLUDE-REMAINER is nil and last element in the result list
has length shorter than N, this last element is discarded."
  (assert (plusp n)) ;; otherwise we'll fall into an infinite loop
  (loop for x on list by (bind #'nthcdr n 1)
	collect (butlast* x (- n)) into res
	finally return (if (nor include-remainer
				(zerop (mod (length list) n)))
			 (butlast res) res)))
;;(cut (0-n 5) 3 nil)
;;(cut '(1 2 3 4 5) 2 t)

(cl-defun positions-if (predicate sequence &key from-end (start 0) end key)
  (loop for start* = start then (1+ pos)
	for pos = (position-if predicate sequence :from-end from-end :start start* :end end :key key)
	while pos collect pos))
;;(positions-if #'oddp (vector 0 1 2 3 4 5) :start 2)

(defun cut-list-if (predicate list inclusion &rest args)
  "Cut LIST in sublists where PREDICATE is true.
If INCLUSION is not nil, then the element in LIST matching
PREDICATE is included in the result."
  (loop for start = 0 then end
	for end in (apply #'positions-if predicate list args)
	if (subseq list start end) collect it into res
	finally (return (if inclusion
			  (append res (list (subseq list (or end 0))))
			  (mapcar #'(lambda (x) (subseq x 1))
			    (append res (list (subseq list (or end 0)))))))))
;;(cut-list-if #'primep (0-n 10) t)
(cl-indent 'cut-list-if 'prog1)

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

(cl-defun combine2 (list1 list2 &optional ordered)
  (if ordered
    (nconc (combine2 list1 list2)
	   (combine2 list2 list1))
    (loop for x1 in (listify list1) nconc 
	  (loop for x2 in (listify list2)
		collect (list x1 x2)))))
;;(combine2 '(a) '())
;;(combine2 '(a) '(b))
;;(combine2 '(a b) '(c d e) t)
;;(combine2 '(a b) '(c d e) t)
;;(combine2 '(a (b c)) '(c d) t)
;;(combine2 '(a b) nil t)

(cl-defun combine-1 (lists)
  "Generalization of combine2"
  (case (length lists)
    (0 nil)
    (1 (list (listify (first lists))))
    (2 (combine2 (first lists) (second lists)))
    (t  (loop for (x y) in (combine2 (first lists)
				  (combine-1 (rest lists)))
	   collect (cons x y)))))
;;(combine-1 '((a b) c (d e)))

(cl-defun combine (lists &key key)
  "Generalization of combine2"
  (if key
    (mapcar (bind #'apply key 1) (combine-1 lists))
    (combine-1 lists)))
;;(combine '((0 1 2) (("3" 1) ("7" 1))))

(defun test-arguments (fn args)
  (combine args :key #'(lambda (&rest args)
			 (format "%S: %S" args (apply fn args)))))
;;(test-arguments #'concat '(("a" "b") ("c") ("e" "f")))

(cl-defun accumulate-sorted-list (list &optional (test #'eql) (min-occurences 1))
  "Accumulates sorted LIST. See `accumulate-list' for details."
  (loop for x in (group list :test test)
	for l = (length x)
	if (>= l min-occurences) collect (list (first x) l)))
;;(accumulate-sorted-list '(a a a b b c d c)) ==> ((a 3) (b 2) (c 1) (d 1) (c 1))
;;(accumulate-sorted-list '(a a a b b c d c) 'equal 2)

(cl-defun accumulate-list (list &optional (test #'<) (min-occurences 1))
  "Returns a list of pairs (X count-X), where X is an element in
list and count-X is the number of occurrences of X. Note that the
resulting list is sorted on the value of COUNT-X"
  (accumulate-sorted-list (sort list test) (lt-equal test) min-occurences))
;;(accumulate-list '(a b c a b a d) #'symbol<) ==> ((a 3) (b 2) (c 1) (d 1))
;;(accumulate-list '(a b c a b a d) #'symbol< 2)

(cl-defun repetitions-1 (list &optional (start-index 0) (test #'eql))
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
  (loop for x on list 
	for i from 0
	if (repetitions-1 x i test)
	append (accumulate-sorted-list it #'equal)))
;;(repetitions '(a a a a a a)) => (((0 1) 5) ((1 1) 4) ((2 1) 3) ((3 1) 2) ((4 1) 1))
;;(repetitions '(a b a b a))
;;(loop for i below 10 do (repetitions '(a b a b a)))

(defun x-repetitions (list)
  "A Q&D version of `repetitions'. Its algorith is far simpler in
code, but is much slower (O(n^3)). Also, the result is not so
informative."
  (loop for i from 1 to (floor (length list) 2)
	append (loop for k below i
		     for shifted-list on list
		     for i-tuples = (cut shifted-list i)
		     append (accumulate-sorted-list i-tuples #'equal 2))))
;;(x-repetitions '(a b a b a b))
;;(x-repetitions (0-n 500))
;;(repetitions (0-n 500))

;;; List functions
(defun swap-head (list predicate)
  "Swaps first element in LIST with the first element in that
matches PREDICATE"
  (awhen (member-if predicate list)
    (rotatef (first it) (first list))
    list))
;;(dv-swap-head '(4 3 2 1) (bind #'< 4))

(defmacro mrotate-list (list &rest positions)
  "Swaps Ith and Jth elements in LIST"
  (let ((glist (gensym)))
    `(let ((,glist ,list))
       (rotatef ,@(mapcar #'(lambda (x) `(nth ,x ,glist)) positions))
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
  (nrotate-list (copy-list list) n))

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

(defun butlast* (list &optional n)
  "Same as `butlast' but accepts negative argument, meaning counting from start."
  (butlast list (mod (or n 1) (length list))))
;;(butlast* '(a b c d e) -2)

(defun nlist-split (list n)
  "Splits LIST into two at position N. Destructive"
  (if (zerop n)
    (list nil list)
    (list (butlast* list (- n))
	(nthcdr n list))))
;;(loop for pos in (a-b -1 3) collect (nlist-split '(a b) pos))

(defun list-split (list n)
  "Splits LIST into two at position N. Non-destructive.
If N is nil the list is not split, so the function returns a list of only LIST"
  (if n
    (nlist-split (copy-list list) n)
    (list list)))
;;(setq qwe '(a b c d e))
;;(list-split qwe 0)

(defun list-split-if (predicate list &rest args)
  "Splits LIST into two at the positions where unary PREDICATE is true
Currently support one split only"
  (aif (apply #'cl-position-if predicate list args)
    (list-split list it)
    (list list)))
;;(list-split-if #'oddp '(1 2 3 4 5))

(defun nsplit-nth (n list)
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
    (loop for i below n
	  collect (let ((list* (copy-list list)))
		    (values (nsplit-nth i list*) list*)))))
;;(test-split-nth) => (((0 (1 2)) (0 1 2)) ((1 (0 2)) (0 2)) ((2 (0 1)) (0 1)))

(defun split-at-pos (list &rest positions)
  "Return a list consisting of the elements of LIST at POSITIONS
together with the remainder of LIST. Destructive."
  (loop for pos in (reverse positions)
	for (x list*) = (nsplit-nth pos list) then (nsplit-nth pos list*)
	collect x into elts
	finally return (list (nreverse elts) list*)))
;;(let ((list '(0 1 2 3 4 5 6 7))) (list (split-at-pos list 1 3 5) list))

(cl-defun filter-duplicates (list1 list2 &key (test #'eql) (start 0) end)
  "Removes all elements from row and fasit that are equal and is
in the same position."
  (loop for elt1 in (subseq list1 start end)
	for elt2 in (subseq list2 start end)
	unless (funcall test elt1 elt2)
	collect elt1 into list1* and collect elt2 into list2*
	finally return (list list1* list2*)))
;;(filter-duplicates '(1 2 3 4) '(1 4 2 3) :test #'(lambda (x y) (eql (oddp x) (oddp y)))) ;;filters equal parity

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

(defun vlist< (l1 l2)
  "A simple, but much slower version of `version-list-<'. The
functions is meant as an example of how to use pad-lists. Btw, it
would be interesting to see why the running times are so extremely
different, see test cases below."
  (apply #'list< (pad-lists (list l1 l2))))
;;(time (loop repeat 100000 do (vlist< '(1 2) '(1 1))))
;;(time (loop repeat 100000 do (version-list-< '(1 2) '(1 1))))


;;;; Sorted trees
(defun insert-sorted-tree (x tree)
  (if tree
    (if (< x (first tree)) ;seach left tree
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
    (loop for i in (list 14 7 11 5 7 3)
	  do (insert-sorted-tree i tree))
    (remove-minimum-element-sorted-tree tree)
    tree))
;;(test-insert-sorted-tree)

(defun remove-minimum-element-sorted-tree (tree)
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

(defun mapcol (function colpos table)
  "Returns a copy of table, but where the column at COLPOS (only) is mapped by FUNCTION.
The TABLE must be a tree, i.e. a list of lists."
  (mapcar #'(lambda (x) 
	      (let ((x* (copy-list x)))
		(asetf (nth colpos x*) (funcall function it))
		x*))
	  table))

(defun expand-repeat (list n)
  (destructuring-bind (a b) (cl-floor n (length list))
    (append list (flatten (make-list a list)) (subseq list 0 b))))
;;(expand-repeat '(1 2 3 4) 6)

(defun diffs-list (list)
  (loop for (a b) in (pairs list) collect (- b a)))
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

(defun deltas-list (floats)
  "Finds the mid-points between FLOATS and returns the interval
consisting of each floats neighbouring mid-points."
  (pairs (loop for (a b) in (pairs floats) collect (/ (- b a) 2.0)) :flank-p t))
;;(deltas-list '(1 3 4))

(defun boundaries-1 (list)
  (loop for (a b) in (pairs list) collect (/ (+ b a) 2)))
;;(boundaries-1 '(1.0 3.0 4.0))

(defun boundaries (list &optional flank-p)
  (boundaries-1 (if flank-p
		  (nflank (- (* 2 (car list)) (cadr list))
			  list
			  (- (* 2 (car (last list))) (car (last list 2))))
		  list)))
;;(boundaries (list -4.0 -3.0 -1.0) t)

(defun randomize-intervals (pairs)
  (loop for (a b) in pairs collect (random-float a b)))
;;(randomize-intervals (pairs (0-n 5)))

(cl-defun randomize-elements (sequence)
  (let ((ivs (pairs (boundaries (coerce sequence 'list) t))))
    (values (coerce (randomize-intervals ivs) (type-of sequence))
	    ivs)))
;;(randomize-elements (vector -4.0 -3.0 -1.0))

(defun group-consequtive-integers (integers &optional sorted-p)
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

(cl-defun relations->tree (relations &key (test #'eql) reverse-p)
  "Given list RELATIONS, return a list of family trees.
A RELATION is a pair (X Y), meaning that X is the parent of the
child Y. The resulting FAMILY-TREE is a CONS based tree where
each subnode SN of a node N is a relation in list RELATIONS. If
instead optional parameter REVERSE-P is not nil then Y is
interpreted as the parent of X."
  (let ((nodes (mapcar #'list
		 (cl-remove-duplicates (flatten relations) :test test))))
    ;; the loop builds child node lists and returns non-top nodes
    (let ((x (loop for (p c) in (if reverse-p
				  (mapcar #'reverse relations) relations)
		   for pn = (cl-find p nodes :key #'car :test test)
		   for cn = (cl-find c nodes :key #'car :test test)
		   do (push cn (cdr pn))
		   collect cn)))
      ;; remove non-top nodes from NODES, and we are home
      (cl-set-difference nodes x))))
;;(relations->tree '((a b) (a c) (c d) (b e)))
;;(relations->tree '((a b) (a c) (c d) (b e)) :reverse-p t)
;;(relations->tree '((a b) (a c) (c d) (b e) (f g)))

(cl-defun tree-member (x tree &key (test #'eql) key from-end)
  "Return first subtree in TREE having X has its top node"
  (if (funcall test x (if key (funcall key (car tree)) (car tree)))
    tree
    (loop for cn in (if from-end (reverse (cdr tree)) (cdr tree))
	  if (tree-member x cn :test test :key key :from-end from-end)
	  return it)))
;;(tree-member 'b '(a (b (c)) (b (d))) :from-end t)

(cl-defun prune-tree-to-target (x tree &key (test #'eql) key from-end)
  "Remove all nodes in FAMILY-TREE not having X as successor.
Note that the keyword :FROM-END never used in this version."
  (when from-end
    (warning "prune-tree-to-target warning: :from-end was set. No effect."))
  (when (tree-member x tree :test test :key key)
    (cons (car tree)
	  (loop for sn in (cdr tree)
		if (prune-tree-to-target x sn :test test :key key)
		collect it))))
;;(prune-tree-to-target 'b '(a (b (c)) (d (b))))

(cl-defun find-subtree (x trees
			  &key reverse-p (test #'eql) key from-end tree-from-end)
  "Find the first subtree in TREES with X as top node.
If REVERSE-P is not nil, return the minimal ancestor tree
containing X."
  (loop for ft in (if from-end (reverse trees) trees)
	if (funcall (if reverse-p #'prune-tree-to-target #'tree-member)
	     x ft :test test :key key :from-end tree-from-end)
	return it))

(cl-defun copy-subtree (x trees
			  &key reverse-p (test #'eql) key from-end tree-from-end)
  "Find the first subtree in TREES with X as top node.
If REVERSE-P is not nil, return the minimal ancestor tree
containing X."
  (loop for ft in (if from-end (reverse trees) trees)
	if (funcall (if reverse-p #'prune-tree-to-target #'tree-member)
	     x ft :test test :key key :from-end tree-from-end)
	collect it))
;;(find-subtree 'e '((f (g)) (a (c (d)) (b (e)))) :reverse-p t)

(defun tree-leaves (tree)
  (when tree
    (aif (cdr tree)
      (loop for x in it append (tree-leaves x))
      (list (car tree)))))
;;(tree-leaves '(a))
;;(tree-leaves '(a))
;;(tree-leaves '(a (c (d (f))) (b (e))))

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

(provide 'mb-lists)
