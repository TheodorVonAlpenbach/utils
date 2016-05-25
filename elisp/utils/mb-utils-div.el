(require 'cl)
(require 'mb-indent)

(cl-defun tmap-n (item n table &key (test #'eq)) (find item table :key #'(lambda (row) (nth n row)) :test test))
(cl-defun tmap-0 (item table &key (test #'eq)) (tmap-n item 0 table :test test))
(cl-defun tmap-1 (item table &key (test #'eq)) (tmap-n item 1 table :test test))
(cl-defun tmap-0-1 (item table &key (test #'eq)) (tmap-n-m item 0 1 table :test test))
(cl-defun tmap-1-0 (item table &key (test #'eq)) (tmap-n-m item 1 0 table :test test))
(cl-defun tmap-n-m (item n m table &key (test #'eq)) (nth m (tmap-n item n table :test test)))

(defun insertf (&rest args) (insert (apply #'format args)))

(defmacro popf (property-list tag)
  "Pops TAG and its value from PROPERTY-LIST"
  `(prog1 (getf ,property-list ,tag)
     (remf ,property-list ,tag)))
;;(let ((props (list :qwe 1 :ewq 2))) (list (popf props :qwe) props))

(defmacro mdelf (place &rest tags)
  "Delete TAGS from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by
`setf'. The form returns a list of booleans indicating if the
corresponding TAG was found and removed.
\nSee also `mremf', `remf'"
  `(loop for tag in ',tags collect (cl-remf ,place tag)))
;;(let ((p '(:where 123 :columns 321))) (list (mdelf p :where :columns :fitna) p))

(defun mremf (plist &rest tags)
  "Remove the TAGS from property list PLIST.
\nSee `delf' for a similar destructive version."
  (let ((place (copy-list plist)))
    (loop for tag in tags do (cl-remf place tag))
    place))
;;(let ((p '(:where 123 :columns 321))) (list (mremf p :where :columns :fitna) p))

(defun remf* (&rest args)
  (warn "This function has been deprecated. Use mremf instead.")
  (apply #'mremf args))
;;(remf* '(:where 123 :columns 321 :qwe) :where :columns :fitna)

(defun mapprop (proplist propnames)
  (mapcar (bind #'getf proplist 1) propnames))
;;(mapprop '(:a 1 :b 2) '(:a :a :b))

(defun smart-insert (&rest line)
  "Opens a new line, inserts any number of strings or characters, and
indents it."
  (interactive)
  (apply #'insert line)
  (indent-according-to-mode)
  (newline))

;; boolean
(defun eq* (&rest args) 
  "Returns the common value if all ARGS are EQ, else nil.
If no arguments is given, t is returned."
  (if (not args) t
    (if (not (rest args)) (first args)
      (if (eq (first args) (second args))
	  (apply 'eq* (rest args))
	nil))))
;;(eq* 2 1 2 2 )

(defun neq (x y) (not (eq x y)))

(defun neq* (&rest args)
  "Returns T if not all ARGS are EQ, else NIL."
  (not (apply 'eq* args)))
;;(neq* 1 2)

(defun nequal (x y) (not (equal x y)))

(defmacro nand (&rest conditions) "Nand." `(not (and ,@conditions)))
(defmacro nor (&rest conditions) "Nor." `(not (or ,@conditions)))
(def-edebug-spec nor t)
(defmacro xor (a b) "Exclusive or." `(if ,a (not ,b) ,b))
(defmacro xnor (a b) "Exclusive nor." `(not (xor ,a ,b)))

;; push/pop
(cl-defmacro pop* (place &optional (n 1) reverse)
  "Pops n times from list at PLACE and returns last element popped.
If reverse i non nil, it returns the first popped element"
  `(prog1 
       (if ,reverse 
	 (car ,place)
	 (nth (1- ,n) ,place))
     (setf ,place (nthcdr ,n ,place))))
;;(setq qwe '(1 2 3 4 5 6))
;;(pop* qwe 3 t)

(defmacro push-back (x place)
  `(setf ,place (nconc ,place (list ,x))))
;;(setf qwe '(a))
;;(push-back 1 qwe)

(defmacro push-list (list place)
  `(setf ,place (nconc ,list ,place)))
;;(setq qwe '(a))
;;(push-list '(1 234 4) qwe)

(cl-defmacro push* (place &rest elts)
  `(push-list (list ,@elts) ,place))
;;(setq qwe '(a))
;;(push* qwe 1 234 4)

(cl-defmacro pop-list (place &optional (n 1) (reverse nil))
  "Pops n times from list at PLACE and returns all the popped
elements as a list maintaing the order."
  (let ((glist (gensym)))
    `(let ((,glist ()))
       (dotimes (i ,n)
	 (push (pop ,place) ,glist))
       (if ,reverse ,glist (nreverse ,glist)))))
;;(let ((qwe '(1 2 3 4))) (list (pop-list qwe 2 t) qwe))

(cl-defmacro pop-until (place &optional (elt (first place)) test)
  "Pops list place containing all top elements not equal to elt"
  (let ((gelt (gensym))
	(gtest (gensym)))
    `(let ((,gelt ,elt)
	   (,gtest ,test))
       (loop for x in ,place
	     while (not (funcall (or ,gtest #'eq) x ,gelt))
	     collect (pop ,place)))))
;;(let ((qwe '(1 2 3 4))) (list (pop-until qwe 3) qwe))

(defmacro nor (&rest args) `(not (or ,@args)))
;;(nor nil nil t)

(defun min* (&rest args)
  (minimum args #'<))
;;(min* 1 2 3)

(defun max* (&rest args)
  (minimum args #'>))
;;(max* 1)

;;; Function objects
(defun always (&rest args) "Returns t, ALWAYS" t)

(defun complement (function)
  "Returns a function that is the complement of FUNCTION"
  (lexical-let ((x function))
    #'(lambda (&rest arguments) (not (apply x arguments)))))
;;(mapcar (complement #'oddp) (0-n 4))

(cl-defun bind (function fixed-argument &optional (floating-argument-position 0))
  "Changes a binary FUNCTION(x,y) to the unary FUNCTION2(y) by
binding first variable x to FIXED-ARGUMENT. If
FLOATING-ARGUMENT-POSITION is set to 1 then y is fixed to
FIXED-ARGUMENT and methods becomes FUNCTION3(x).

Could be extended to take multiple FIXED-ARGUMENTS and likewise
multiple FLOATING-ARGUMENT-POSITIONS. Then one could tranform
FUNCTION(x,y,z,w,v) to FUNCTION3(x,z) = FUNCTION(x,a,z,b,c),
where '(a b c) are the FIXED-ARGUMENTS and '(1 3 4) are the
FLOATING-ARGUMENT-POSITIONS.

TODO: parse FUNCTIONs argument list and return a function accordingly.
Also take an argument FIXED-ARGUMENTS and and optional FLOATING-ARGUMENT-POSITIONS"
  (lexical-let ((f function)
		(a fixed-argument)
		(pos floating-argument-position))
    (function 
     (lambda (x)
       (if (= pos 0)
	 (funcall f x a)
	 (funcall f a x))))))

(cl-defun bind* (function fixed-arguments &optional fixed-argument-positions)
  "Changes FUNCTION(args optional-args) to FUNCTION2(args*
optional-args*) by binding a subset of args to the
FIXED-ARGUMENTs. By default the head of args is bound, but thi
can be overriden by providing the list FIXED-ARGUMENT-POSITIONS
with the indexes of the args to be bound by FIXED-ARGUMENTS.

If only one argument is to be bound, fixed-arguments (and
fixed-argument-positions) could be provided as an atom for
simplicity: 
\(bind* #'length \"qwe\") => (lambda () (length \"qwe\"))
\(funcall (bind* #'length \"qwe\")) => 0
\(bind* #'/ 2 1) => (lambda (dividend &rest divisors) (apply #'/ dividend 16 divisors))
\(funcall (bind* #'/ 2 1) 16 4 2) => 1
\(funcall (bind* #'/ 2 0) 16.0 4 2) => 0.015625 ie. (/ 2 16.0 4 2)

TODO: implement this. Probably involves some macro magic"
  (lexical-let ((f function)
		(fargs (llist fixed-arguments))
		(positions (llist fixed-argument-positions)))
    (function 
     (lambda (&rest args)
       (let* ((args* (copy-list args))
	      (all-args (if (first positions)
			  (loop for i in positions 
				for x in fargs
				do (list-insert x i args*)
				finally return args*)
			  (append args* fargs))))
	 (apply f all-args))))))
;;(funcall (bind* #'length ""))
;;(funcall (bind* #'concat '("a" "b") '(0 1)) "c")
;;(mapcar (bind #'nth '(a b c) 0) '(0 2 1))

(cl-defun compose (&rest fns)
  (lexical-let ((fns fns))
    (if fns
	(lexical-let ((fn1 (car (last fns)))
		      (fns (butlast fns)))
	  (function (lambda (&rest args)
		      (reduce #'funcall fns 
			      :from-end t
			      :initial-value (apply fn1 args)))))
      (function identity))))
;;(funcall (compose #'1+ #'sq) 1)

(cl-defun arg-map (function &rest args)
  (funcall #'mapcar function args))
;;(arg-map #'oddp 1 2 3 4)

(cl-defun arg-apply (function &rest args)
  (apply function args))
;;(arg-apply (compose #'+) 1 2)

(defun lt-equal (less-than-function)
  "Returns the equality induced by the ordering LESS-THAN-FUNCTION"
  (lexical-let ((lt less-than-function))
    #'(lambda (&rest args)
      (nor (apply lt args) (apply lt (nreverse args))))))
;;(funcall (lt-equal #'-<) 1 1 1)

;;; Anaphoric macros
(defmacro aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
;(aif (+ 5 5) it nil 1)
;(aif nil it 1 2 3)
(def-edebug-spec aif t)

(cl-defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))
;(awhen (+ 2 2) (princ (1+ it)) (princ (1- it)) (princ " cool!"))
(def-edebug-spec awhen t)

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))
(def-edebug-spec acond 'cond)

(defmacro nilf (&rest args)
  "Sets all ARGS to nil."
  `(progn (setf ,@(mapcan #'(lambda (x) (list x nil)) args))))
;;(macroexpand '(nilf (first c) b))

(defmacro minf (place &rest numbers)
  "Sets PLACE to the minimum value of itself and each of NUMBERS."
  `(progn (setf ,place (min ,place ,@numbers))))
;;(let ((a 1)) (list (minf a 0 3) a))

(defmacro maxf (place &rest numbers)
  "Sets PLACE to the maximum value of itself and each of NUMBERS."
  `(progn (setf ,place (max ,place ,@numbers))))
;;(let ((a 1)) (list (maxf a 0 3) a))

(defmacro asetf (place expression)
  `(let ((it ,place))
    (setf ,place ,`,expression)))
;;(setf ewq '((1) 1)) ==> ((1) 1)
;;(asetf (first ewq) (* 2 (first it))) ==> 2
;;ewq ==> (2 1)

(defun -< (first &rest rest)
  "Returns nil iff not the elements in REST are in ascending order
due to operator #'<"
  (or (null rest)
      (and (< first (car rest))
           (apply #'-< rest))))
;;(-< 1 2 3 4 3) 

(defun -= (first &rest rest)
  "Returns nil iff not the elements in REST are equal
due to operator #'="
  (or (and (null rest) 
	   (numberp first))
      (and (= first (first rest))
	   (apply #'-= rest))))
;;(-= 1 1 1)

(defun -<= (first &rest rest)
  "Returns nil iff not the elements in REST are in descending order
due to operator #'>"
  (or (null rest)
      (and (or (< first (car rest)) (= first (car rest)))
           (apply #'-<= rest))))
;;(-<= 1 2 3 3 2)

(defmacro definteractive (defun)
  "Make DEFUN interactive with name CL-DEFUN. DEFUN takes no arguments.
TODO: font-lock face as `defun'."
  `(defun ,(intern (concat (symbol-name defun) "*")) ()
    (interactive)
    (princ (,defun))))
;(definteractive1 qwe)
(definteractive point)
(definteractive point-min)
(definteractive point-max)

(defun vxw (v w) 
  (let ((res nil))
    (loop for ev in v
	  do (loop for ew in w
		   do (push (list ev ew) res)))
    (nreverse res)))
;;(vxw (0-n 2) (1-n 3))

(defmacro with-gensyms (syms &rest body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))
;;(with-gensyms (gx gy) 1)

(defun last-elt (sequence)
  "Return last element of SEQUENCE."
  (elt sequence (1- (length sequence))))

(defun first-elt (sequence)
  "Return first element of SEQUENCE."
  (and (plusp (length sequence))
       (elt sequence 0)))

;;; since elisp's let sucks...
(cl-defmacro with-variable ((variable &optional temp-value) &body body)
  "`setf's VARIABLE to TEMP-VALUE and executes BODY. Then VARIABLE's
value is reset."
  (let ((old-value (gensym)))
    `(let ((,old-value ,variable))
      (setf ,variable ,temp-value)
      (unwind-protect
	(progn ,@body)
	(setf ,variable ,old-value)))))
;;(with-variable (qwe 11) qwe)

(cl-defmacro with-constant-match-data (&body body)
  "`setf's VARIABLE to TEMP-VALUE and executes BODY. Then VARIABLE's
value is reset."
  (let ((old-match-data (gensym)))
    `(let ((,old-match-data (match-data)))
      (unwind-protect
	(progn ,@body)
	(set-match-data ,old-match-data)))))

(cl-defun constantly (x) (lexical-let ((x x)) #'(lambda (&rest args) x)))

(cl-defmacro with-syntax-table (table &body body)
  "See doc at emacs lisp info."
  (let ((old-table (gensym)))
    `(let ((,old-table (syntax-table)))
       (set-syntax-table ,table)
       (unwind-protect
	 (progn ,@body)
	 (set-syntax-table ,old-table)))))

(defun assoc-project (sequence a b)
  "Map each sequence element ELT of SEQUENCE to (cons (elt ELT A) (elt
ELT B))."
  (mapcar #'(lambda (x) (cons (elt x a) (elt x b))) sequence))
;;(assoc-project '((a aa aaa) (b bb bbb)) 0 1)

(defun project (sequence projection)
  "Project SEQUENCE according to integer sequence PROJECTION. See
#'PROJECT. Maybe this is obsolete. Must check its use. Or maybe call."
  (if (eql projection t)
    sequence
    (coerce (loop for x in (coerce projection 'list)
		  collect (if (integerp x)
			    (elt sequence x)
			    (funcall x sequence)))
	    (type-of-super sequence))))
;;(project '(a b c) '(1 0 2 1))
;;(project '(a b c) t)
;;(project '(a b c) '(2))

(defun project-sequence (sequence &rest projection-args)
  "Project SEQUENCE according to PROJECTION-ARGS."
  (coerce (mapcar #'(lambda (x) (project x projection-args)) sequence)
	  (type-of-super sequence)))
;;(project-sequence '("01" "09") (list 1))
;;(mapcar #'first (project-sequence '((1 2 3) (1 2 3)) 1))

(cl-defmacro dolines ((line string &optional result) body)
  "(dolines (LINE STRING [RESULT]) BODY...): loop over lines in a
string. Evaluate BODY with LINE bound to each consecutive substring in
STRING ending with a newline character, in turn. Then evaluate RESULT
to get return value, default nil."
 `(dolist (,line (string-to-lines ,string) ,result) ,@body))
;;(dolines (l (buffer-string)) )

(cl-defun group-list (source &optional (n 2))
  "Obsolete.  See `cut'"
  (error "Obsolete, use cut instead"))

(defun rmapcar (fn &rest args)
  "A recursive generalization of `mapcar*'"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar* 
             #'(lambda (&rest args) 
                 (apply #'rmapcar fn args))
             args)))
;;(rmapcar #'+ '(1 (1) 1) '(1 (1) 1))
;;(rmapcar #'+ 1 1)

(cl-defun flatten (x &optional (levels most-positive-fixnum))
  "Flattens out all arguments in tree X. If optional LEVELS is a
number, only flatten down this many tree levels."
  (if (and x (atom x))
    (list x)
    (if (plusp levels)
      (mapcan (bind #'flatten (1- levels)) x)    
      x)))
;;(flatten '((1) 2 (2 (3))))
;;(flatten '(nil))

(defun assocp (x) (and (consp x) (not (listp (cdr x)))))
;;(assocp '(1 . 2))

(defun mapassoc (fn x)
  (cons (funcall fn (car x)) (funcall fn (cdr x))))
;;(mapassoc #'1+ '(1 . 2))

(cl-defun maptree (function tree &key (levels most-positive-fixnum) (with-assoc nil))
  "Maps TREE to another three with same structure applying
FUNCTION If optional argument LEVEL is provided, the mapping goes
only this number deep in TREE. If LEVEL is 1 the method is
similar to `mapcar'"
  (if (and (listp tree) 
	   (plusp levels))
    (if (and with-assoc (assocp tree))
      (mapassoc function tree)
      (loop for x in tree
	  collect (maptree function x :levels (1- levels) :with-assoc with-assoc)))
    (funcall function tree)))

(cl-defun maptree* (function tree &rest trees)
  "Same as `maptree' but allows for at n-ary FUNCTION. Assumes
the structures of TREES are the same as for TREE."
  (if (listp tree)
    (loop for i below (length tree)
	  collect (apply #'maptree* function
			 (cons (nth i tree)
			       (mapcar (bind #'nth i 1) trees))))
    (apply function tree trees)))
;;(maptree* #'+ 1 2 3 4)
;;(maptree* #'+ '(1 2) '(2 3) '(3 4) '(4 5))
;;(maptree* #'+ '((1 2)) '((2 3)) '((3 4)) '((4 5)))

(cl-defun mapply (function list)
  "Same as mapcar, but assumes each element in LIST is a list of
arguments to FUNCTION"
  (mapcar (bind #'apply function 1) list))
;;(mapply #'+ '((1 2) (3 4)))

(defun mapfun (function-list &rest args)
  "Maps FUNCTION-LIST = (fn1 fn2 ...) and ARGS = arg1 arg2 ... to
the list ((fn1 arg1 arg2 ...) (fn2 arg1 arg2 ...) ...)"
  (mapcar (bind #'apply args) function-list))
;;(mapfun (list #'+ #'* #'list) 2 3) => (5 6 (2 3))

(defun not-null (x) (not (null x)))

(cl-defun position-num (cl-item cl-seq &rest cl-keys)
  "Same as `POSITION', but returns always a number, even if CL-ITEM is
not found. Instead of returning nil, the length of CL-SEQ is returned.
It is useful in connection with functions like `SUBSEQ', `SUBSTRING',
etc. NB! Check if obsolete!"
  (or (apply #'position cl-item cl-seq cl-keys)
      (length cl-seq)))

(cl-defmacro push-unique (x place &optional test)
  "Like `push', but pushes X on PLACE only if PLACE does not contain X."
  `(if (member* ,x ,place :test ,(or test `#'eql))
     ,place
     ,(if (symbolp place)
      `(setq ,place (cons ,x ,place))
      `(callf2 cons ,x ,place))))
;;(push-unique (1 1) #'equal)

(defmacro modf (x y)
  `(setf ,x (mod ,x ,y)))
;;(let ((x 11)) (modf x 6) x)


(defun struct-name (x)
  (if (eq (type-of x) 'vector)
    (let* ((px (split-string (format "%S" x))))
      (substring (first px) (1+ (length "cl-struct-"))))
    (first x)))
;;(defstruct qwe)
;;(struct-name (make-key))
;;(intern-soft "qwe")

(defun struct-type (x)
  (intern-soft (struct-name x)))
;;(defstruct qwe)
;;(struct-type (make-qwe))

(defun equal* (&rest args)
  (message "Warning! this function is deprecated. Use `all-equal' instead")
  (apply #'all-equal args))
;;(equal* 1 1)

(defun all-equal (&rest args)
  (> 2 (length (remove-duplicates args :test #'equal))))
;;(all-equal 1 1)

(defun mequal (map-function &rest args)
  "Tests whether all ARGS are EQUAL after applying MAP-FUNCTION
on them. TODO: somehow make test function (EQUAL) configurable"
  (apply #'all-equal (mapcar map-function args)))
;;(mequal #'oddp 1 3)

(cl-defun all-true (&rest args)
  (notany #'null args))
;;(all-true t t)

(defun function-signature (function)
  "Returns FUNCTION's signature"
  (second (symbol-function function)))
;;(function-signature 'test-function-special-args)

(defun test-function-special-args (a &optional b &keys c &rest d))

(cl-defun function-args (function &optional (argument-type t))
  "Returns the FUNCTION's' arguments.
If ARGUMENT-TYPE is one of '&optional '&keys '&rest (note the
quote), only the respective arguments belonging to this sections
are returned. If it is nil, only the fixed arguments are
returned. If t (default), then all arguments."
  (let* ((signature (function-signature function))
	 (special-argument-types '(&optional &keys &rest))
	 (start-pos) (end-pos))
    (cond
     ((eq argument-type t)
      (remove-if #'(lambda (x) (member x special-argument-types))
		 signature))

     ((eq argument-type nil)
      (subseq signature 
	0 
	(position-if #'(lambda (x) 
			 (member x special-argument-types))
		     signature)))
     (t (setq signature (member argument-type signature))
	(let ((end-pos (or (position-if #'(lambda (x)
					    (member x (remove argument-type special-argument-types)))
					signature)
			   (length signature))))
	  (subseq signature 1 end-pos))))))
;;(function-args 'test-function-special-args '&keys)
;;(function-args 'test-function-special-args t)
;;(function-args 'test-function-special-args '&keys)

(defun re-evaluate-function (function-symbol)
  (let* ((file-orig (symbol-file function-symbol 'defun))
	 (file (if (string-match "\\.elc$" file-orig)
		 (substring* file-orig 0 -1)
		 file-orig)))
    (when file
      (with-file-readonly file
	(re-search-forward (concat "^" (regexp-quote (format "(defun %s " (symbol-name function-symbol)))))
	(eval-defun nil)))))

(cl-defun sstring (string-designator &optional (nil-string nil) (integer-is-char-p nil))
  "Converts STRING-DESIGNATOR to a string.
Strings are unchanged. Symbols and keywords are converted to
their corresponding names. Positive integers are interpreted as
characters if INTEGER-IS-CHAR-P is non-nil, and then they are
converted to the corresponding strings of length 1. Otherwise
STRING-DESIGNATOR is converted as with the %S argument in
`format' If STRING-DESIGNATOR evaluates to nil, the result is
converted to optional argument NIL-STRING."
  (if string-designator
    (cond ((stringp string-designator) string-designator)
	  ((and integer-is-char-p
		(integerp string-designator)
		(not (minusp string-designator)))
	   (string string-designator))
	  ((keywordp string-designator) (keyword-name string-designator))
	  (t (format "%S" string-designator)))
    nil-string))
;;(loop for x in '(nil :qwe qwe "qwe" 123 -1 (:read 2 3)) collect (sstring x 0 t))

(defun iintern (symbol-name)
  (if (stringp symbol-name)
    (if (empty-string-p symbol-name)
      nil
      (if (integer-string-p symbol-name)
	(integer-to-string symbol-name)
	(intern symbol-name)))
    symbol-name))
;;(mapcar #'symbolp (mapcar #'iintern (list 1 "a" 'b "")))

(defun llist (x)
  (warn "llist is deprecated. Use listify instead")
  (listify x))

(defun listify (x)
  "Coerces x to become a cons. Also a lambda expression, which
also is a cons, will be encapsulated in a list."
  (if (or (not (listp x)) (functionp x))
    (list x) x))
;;(mapcar #'listify (list 'a (list 1 2 3) nil #'(lambda () nil)))


;;; symbols
(defun concat-symbols (symbol &rest symbols)
  (if symbols
    (apply #'concat-symbols 
	   (iintern (concat (sstring symbol) (sstring (first symbols))))
	   (rest symbols))
    symbol))
;;(concat-symbols 'a 1)

(defun symbol-left (symbol N)
  "Abbrevates SYMBOL to its N leftmost characters"
  (intern (substring (symbol-name symbol) 0 N)))
;;(symbol-left 'testtest 2)

(defun symbol< (x y)
  "Apply `string<' on the symbol names of two symbols X Y."
  (string< (sstring x) (sstring y)))
;;(symbol< 'a 'b)

(defun keyword-name (keyword-symbol)
  (if (keywordp keyword-symbol)
    (upcase (substring (symbol-name keyword-symbol) 1))
    keyword-symbol))
;;(keyword-name :key)

(defun make-keyword (name)
  (intern (format ":%s" name)))
;;(make-keyword "qwe")

(defun numcond-transform-clauses (clauses gx gy)
  "TODO: handle t and otherwise like in `cond'"
  `(cond ,@(mapcar #'(lambda (x) `((,(first x) ,gx ,gy) ,@(rest x)))
		       clauses)))
;;(pp (macroexpand '(numcond (3 3) ((= 'eq)))))

(cl-defmacro numcond (arg &rest clauses)
  (when (atom arg)
    (setf arg (list arg 0)))
  (with-gensyms (gx gy) 
    `(destructuring-bind (,gx ,gy) (list ,@arg)
        ,(numcond-transform-clauses clauses gx gy))))
(cl-indent 'numcond 'case)
;;(numcond (4 3) (= 'eq) (< 'lt) (> 'geq))

(provide 'mb-utils-div)
