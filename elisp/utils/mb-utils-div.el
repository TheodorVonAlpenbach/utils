;; -*- lexical-binding: t; -*-
(require 'mb-indent)

(cl-defmacro without-message (&body body)
  `(let ((message-log-max nil))
     (progn ,@body)))
;;(without-message 'qwe)
;;(let ((message-log-max 0)) (eval-defun nil) (eval 2))
;;(let ((qwe nil)) (eval-defun nil) (eval 2))

(cl-defun tmap-n (item n table &key (test #'eq))
  (cl-find item table :key #'(lambda (row) (nth n row)) :test test))
(cl-defun tmap-0 (item table &key (test #'eq)) (tmap-n item 0 table :test test))
(cl-defun tmap-1 (item table &key (test #'eq)) (tmap-n item 1 table :test test))
(cl-defun tmap-0-1 (item table &key (test #'eq)) (tmap-n-m item 0 1 table :test test))
(cl-defun tmap-1-0 (item table &key (test #'eq)) (tmap-n-m item 1 0 table :test test))
(cl-defun tmap-n-m (item n m table &key (test #'eq)) (nth m (tmap-n item n table :test test)))

(cl-defun insertf (&rest args) (insert (apply #'format args)))

(defmacro popf (property-list tag &optional default)
  "Pops TAG and its value from PROPERTY-LIST"
  `(prog1 (cl-getf ,property-list ,tag ,default)
     (cl-remf ,property-list ,tag)))
;;(let ((props (list :qwe 1 :ewq 2))) (list (popf props :qwe) props))

(defmacro mdelf (place &rest tags)
  "Delete TAGS from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by
`setf'. The form returns a list of booleans indicating if the
corresponding TAG was found and removed.
\nSee also `mremf', `remf'"
  `(cl-loop for tag in ',tags collect (cl-remf ,place tag)))
;;(let ((p '(:where 123 :columns 321))) (list (mdelf p :where :columns :fitna) p))

(cl-defun mremf (plist &rest tags)
  "Remove the TAGS from property list PLIST.
\nSee `delf' for a similar destructive version."
  (let ((place (copy-list plist)))
    (cl-loop for tag in tags do (cl-remf place tag))
    place))
;;(let ((p '(:where 123 :columns 321))) (list (mremf p :where :columns :fitna) p))

(cl-defun remf* (&rest args)
  (warn "This function has been deprecated. Use mremf instead.")
  (apply #'mremf args))
;;(remf* '(:where 123 :columns 321 :qwe) :where :columns :fitna)

(cl-defun mapprop (proplist propnames)
  (mapcar (bind #'getf proplist 1) propnames))
;;(mapprop '(:a 1 :b 2) '(:a :a :b))

(cl-defun smart-insert (&rest line)
  "Opens a new line, inserts any number of strings or characters, and
indents it."
  (interactive)
  (apply #'insert line)
  (indent-according-to-mode)
  (newline))

;; boolean
(cl-defun eq* (&rest args) 
  "Returns the common value if all ARGS are EQ, else nil.
If no arguments is given, t is returned."
  (if (not args) t
    (if (not (rest args)) (first args)
      (if (eq (first args) (second args))
	  (apply 'eq* (rest args))
	nil))))
;;(eq* 2 1 2 2 )

(cl-defun neq (x y) (not (eq x y)))
(cl-defun neql (x y) (not (eql x y)))

(cl-defun neq* (&rest args)
  "Returns T if not all ARGS are EQ, else NIL."
  (not (apply 'eq* args)))
;;(neq* 1 2)

(cl-defun nequal (x y) (not (equal x y)))

(defmacro nand (&rest conditions)
  "Return nil if at least one of CONDITIONS is nil, otherwise return t."
  `(not (and ,@conditions)))
(def-edebug-spec nor t)

(defmacro nor (&rest conditions)
  "Return t if all CONDITIONS is nil, otherwise return nil."
  `(and ,@(mapcar #'(lambda (x) `(not ,x)) conditions)))
(def-edebug-spec nor t)

(cl-defun xor (&rest conditions)
  "Return nil iff there are an even number of conditions that evaluates to nil."
  (cl-oddp (cl-count nil conditions)))
;;(xor t nil t)

(cl-defun xnor (&rest conditions)
  "Return nil iff there are an even number of conditions that evaluates to non nil."
  (cl-oddp (count-if (complement #'null) conditions)))
;;(xnor t t nil)

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

(defmacro push-list (list place)
  `(setf ,place (append ,list ,place)))
;;(setq qwe '(a))
;;(push-list '(1 234 4) qwe)

(defmacro pushnew-list (list place &rest args)
  `(cl-loop for x in (reverse ,list)
	 do (cl-pushnew x ,place ,@args)
	 finally return ,place))
;; (let ((l '(c d e))) (progn (pushnew-list '(a b c) l)))
;;(macroexpand-1 '(pushnew-list '(a b c) qwe :test #'string=))

(defmacro push-back (x place)
  `(setf ,place (nconc ,place (list ,x))))

(cl-defun test-push-back1 ()
  (let ((l (list 'a)))
    (list (push-back 'b l) l)))
;;(test-push-back1)

;;(setf qwe '(a))
;;(push-back 1 qwe)

(defmacro push-back-list (list place)
  `(setf ,place (nconc ,place ,list)))
;;(setf qwe '(a))
;;(push-back 1 qwe)

(cl-defmacro push* (place &rest elts)
  "Same as `push' but allows push of multiple elements."
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
       (cl-loop for x in ,place
	     while (not (funcall (or ,gtest #'eq) x ,gelt))
	     collect (pop ,place)))))
;;(let ((qwe '(1 2 3 4))) (list (pop-until qwe 3) qwe))

(defmacro nor (&rest args) `(not (or ,@args)))
;;(nor nil nil t)

(cl-defun min* (&rest args)
  (minimum args #'<))
;;(min* 1 2 3)

(cl-defun max* (&rest args)
  (minimum args #'>))
;;(max* 1)

;;; Function objects
(cl-defun always (&rest args) "Returns t, ALWAYS" t)
(cl-defun never (&rest args) "Returns nil, always" nil)

(cl-defun complement (function)
  "Returns a function that is the complement of FUNCTION"
  (let ((x function))
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
  (let ((f function)
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
  (let ((f function)
		(fargs (listify fixed-arguments))
		(positions (listify fixed-argument-positions)))
    (function 
     (lambda (&rest args)
       (let* ((args* (copy-list args))
	      (all-args (if (first positions)
			  (cl-loop for i in positions 
				for x in fargs
				do (list-insert x i args*)
				finally return args*)
			  (append args* fargs))))
	 (apply f all-args))))))
;;(funcall (bind* #'length ""))
;;(funcall (bind* #'concat '("a" "b") '(0 1)) "c")
;;(mapcar (bind #'nth '(a b c) 0) '(0 2 1))

(cl-defun compose (&rest fns)
  "Return a function that applies functions FNS from right to left."
  (let ((fns fns))
    (if fns
      (let ((fn1 (car (last fns)))
		    (fns (butlast fns)))
	(function (lambda (&rest args)
	  (cl-reduce #'funcall fns 
		  :from-end t
		  :initial-value (apply fn1 args)))))
      (function identity))))
;;(funcall (compose #'sq #'1+) 1)
;;(funcall (compose #'1+ #'sq) 1)

(cl-defun disjoin (&rest predicates)
  "Return a function that returns t if any of PREDICATES return not nil."
  (let ((preds predicates))
    (function (lambda (&rest args)
      (cl-loop for p in preds thereis (apply p args))))))
;;(mapcar (disjoin #'oddp #'primep) (0-n 10))

(cl-defun not-disjoin (&rest predicates)
  "Return a function that returns t if all PREDICATES return nil."
  (complement (apply #'disjoin predicates)))
;;(mapcar (not-disjoin #'evenp #'primep) (0-n 12))

(cl-defun conjoin (&rest predicates)
  "Return a function that returns nil if any of PREDICATES return nil."
  (let ((preds predicates))
    (function (lambda (&rest args)
      (cl-loop for p in preds always (apply p args))))))
;;(mapcar (conjoin #'evenp #'primep) (0-n 10))

(cl-defun not-conjoin (&rest predicates)
  "Return a function that returns nil if all PREDICATES return not nil."
  (complement (apply #'conjoin predicates)))
;;(mapcar (not-conjoin #'evenp #'primep) (0-n 10))

(cl-defun arg-map (function &rest args)
  (funcall #'mapcar function args))
;;(arg-map #'oddp 1 2 3 4)

(cl-defun arg-apply (function &rest args)
  (apply function args))
;;(arg-apply (compose #'+) 1 2)

(cl-defun lt-equal (less-than-function)
  "Returns the equality induced by the ordering LESS-THAN-FUNCTION"
  (let ((lt less-than-function))
    #'(lambda (&rest args)
      (nor (apply lt args) (apply lt (nreverse args))))))
;;(funcall (lt-equal #'<) 1 1 1)

;;; Anaphoric macros
(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric `if'"
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
;(aif (+ 5 5) it nil 1)
;(aif nil it 1 2 3)
(def-edebug-spec aif t)

(cl-defmacro awhen (test-form &body body)
  "Anaphoric `when'"
  `(aif ,test-form
        (progn ,@body)))
;(awhen (+ 2 2) (princ (1+ it)) (princ (1- it)) (princ " cool!"))
(def-edebug-spec awhen t)

(defmacro acond (&rest clauses)
  "Anaphoric `cond'"
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

(defmacro notf (&rest args)
  "Sets each ARG in ARGS to (not ARG)."
  `(progn (setf ,@(mapcan #'(lambda (x) (list x `(not ,x))) args))))
;;(macroexpand '(notf (first c) b))

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

(defmacro interactivate (&rest body)
  `(lambda ()
      (interactive)
      ,@body))
;;(interactivate (list 1))

(defmacro definteractive (defun)
  "Make DEFUN interactive with name CL-DEFUN. DEFUN takes no arguments.
TODO: font-lock face as `defun'."
  `(cl-defun ,(intern (concat (symbol-name defun) "*")) ()
    (interactive)
    (princ (,defun))))
;(definteractive1 qwe)
(definteractive point)
(definteractive point-min)
(definteractive point-max)

(cl-defun vxw (v w &optional (map #'list)) 
  ;; (print (list v w))
  (let ((res nil))
    (cl-loop for ev in v
	  do (cl-loop for ew in w
		   do (push (funcall map ev ew) res)))
    ;; (print (nreverse res))
    (nreverse res)))
;;(vxw (0-n 2) (1-n 3) #'*)

(cl-defun cartesian-product (lists &optional (map #'list))
  (mapply map (cl-reduce #'(lambda (v w) (vxw v w #'append))
		(mapcar #'(lambda (x) (mapcar #'list x)) lists))))
;;(cartesian-product '(("a" "b") ("c" "d") ("e" "f" "g") ("h")) #'concat)

;;(mapcar #'(lambda (x) (mapcar #'list x)) '(("a" "b") ("c" "d") ("e" "f")))
;;(cartesian-product (list (0-n 2) (1-n 3) (a-b 3 4)) #'*)

(defmacro with-gensyms (syms &rest body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))
;;(with-gensyms (gx gy) 1)

(cl-defun last-elt (sequence &optional (n 0))
  "Return last element of SEQUENCE."
  (elt sequence (- (length sequence) 1 n)))
;;(last-elt '(a b c) 1)

(cl-defun first-elt (sequence)
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

(cl-defmacro with-object ((x expr) &body body)
  "Execute BODY with value bound to symbol X, and then return the evaluation of X.
This is useful for such constructs as

\(let ((x expr))
   (setf (third x)) 'qwe)
   x) ==
\(with-object (x expr)
   (setf (third x) 'qwe))"
  `(let ((,x ,expr))
     ,@body
     ,x))
;;(with-object (x '(a b d)) (setf (third x) 'c))
(def-edebug-spec with-object
    ((symbolp form)
     body))

(cl-defun constantly (x)
  (let ((x x)) #'(lambda (&rest args) x)))
;;(mapcar (constantly 1) '(a b c))

(cl-defmacro with-syntax-table (table &body body)
  "See doc at emacs lisp info."
  (let ((old-table (gensym)))
    `(let ((,old-table (syntax-table)))
       (set-syntax-table ,table)
       (unwind-protect
	 (progn ,@body)
	 (set-syntax-table ,old-table)))))

(cl-defmacro dolines ((line string &optional result) body)
  "(dolines (LINE STRING [RESULT]) BODY...): loop over lines in a
string. Evaluate BODY with LINE bound to each consecutive substring in
STRING ending with a newline character, in turn. Then evaluate RESULT
to get return value, default nil."
 `(dolist (,line (string-lines ,string) ,result) ,@body))
;;(dolines (l (buffer-string)) )

(cl-defun group-list (source &optional (n 2))
  "Obsolete.  See `cut'"
  (error "Obsolete, use cut instead"))

(cl-defun rmapcar (fn &rest args)
  "A recursive generalization of `mapcar*'"
  (if (some #'atom args)
      (apply fn args)
      (apply #'cl-mapcar
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

(cl-defun assocp (x) (and (consp x) (not (listp (cdr x)))))
;;(assocp '(1 . 2))

(cl-defun mapassoc (fn x)
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
      (cl-loop for x in tree
	  collect (maptree function x :levels (1- levels) :with-assoc with-assoc)))
    (funcall function tree)))

(cl-defun maptree* (function tree &rest trees)
  "Same as `maptree' but allows for at n-ary FUNCTION. Assumes
the structures of TREES are the same as for TREE."
  (if (listp tree)
    (cl-loop for i below (length tree)
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

(cl-defun mapfun (function-list &rest args)
  "Maps FUNCTION-LIST = (fn1 fn2 ...) and ARGS = arg1 arg2 ... to
the list ((fn1 arg1 arg2 ...) (fn2 arg1 arg2 ...) ...)"
  (mapcar (bind #'apply args) function-list))
;;(mapfun (list #'+ #'* #'list) 2 3) => (5 6 (2 3))

(cl-defun not-null (x) (not (null x)))

(cl-defun position-num (cl-item cl-seq &rest cl-keys)
  "Same as `POSITION', but returns always a number, even if CL-ITEM is
not found. Instead of returning nil, the length of CL-SEQ is returned.
It is useful in connection with functions like `SUBSEQ', `SUBSTRING',
etc. NB! Check if obsolete!"
  (or (apply #'position cl-item cl-seq cl-keys)
      (length cl-seq)))

(cl-defmacro push-unique (x place &optional test)
  "Like `push', but pushes X on PLACE only if PLACE does not contain X."
  `(if (cl-member ,x ,place :test ,(or test `#'eql))
     ,place
     ,(if (symbolp place)
      `(setq ,place (cons ,x ,place))
      `(callf2 cons ,x ,place))))
;;(push-unique (1 1) #'equal)

(defmacro modf (x y)
  `(setf ,x (mod ,x ,y)))
;;(let ((x 11)) (modf x 6) x)

(cl-defun struct-name (x)
  (if (eq (type-of x) 'vector)
    (let* ((px (split-string (format "%S" x))))
      (substring (first px) (1+ (length "cl-struct-"))))
    (first x)))
;;(defstruct qwe)
;;(struct-name (make-key))
;;(intern-soft "qwe")

(cl-defun struct-type (x)
  (intern-soft (struct-name x)))
;;(defstruct qwe)
;;(struct-type (make-qwe))

(cl-defun all-equal (&rest args)
  "Return T if all ARGS are EQUAL and NIL if not."
  (not (and args (some (bind #'nequal (car args)) (cdr args)))))

(cl-defun equal-elements (sequence &optional (test #'equal))
  "Return T if all ARGS are EQUAL and NIL if not."
  (not (and (plusp (length sequence))
	    (some (bind (compose #'not test) (elt sequence 0))
		  (subseq sequence 1)))))
;;(equal-elements [1 1])

(cl-defun mequal (map-function &rest args)
  "Tests whether all ARGS are EQUAL after applying MAP-FUNCTION
on them. TODO: somehow make test function (EQUAL) configurable"
  (apply #'all-equal (mapcar map-function args)))
;;(mequal #'oddp 1 3)

(cl-defun all-true (&rest args)
  (cl-notany #'null args))
;;(all-true t t)

(cl-defun function-signature (function)
  "Returns FUNCTION's signature"
  (second (symbol-function function)))
;;(function-signature 'test-function-special-args)

(cl-defun test-function-special-args (a &optional b &keys c &rest d))

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

(cl-defun re-evaluate-function (function-symbol)
  (let* ((file-orig (symbol-file function-symbol 'defun))
	 (file (if (string-match "\\.elc$" file-orig)
		 (substring* file-orig 0 -1)
		 file-orig)))
    (when file
      (with-file-readonly file
	(re-search-forward (concat "^" (regexp-quote (format "(cl-defun %s " (symbol-name function-symbol)))))
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
	  ((symbolp string-designator) (symbol-name string-designator))
	  (t (format "%S" string-designator)))
    nil-string))
;;(cl-loop for x in '(nil :qwe qwe "qwe" 123 -1 (:read 2 3)) collect (sstring x 0 t))

(cl-defun ssymbol (symbol-designator)
  (acond
    ((or (symbolp symbol-designator)
	 (numberp symbol-designator)) symbol-designator)
    ((stringp symbol-designator) (read symbol-designator))
    ((and (consp symbol-designator)
	  (eql (first symbol-designator) 'quote))
     (eval symbol-designator))))
;;(mapcar #'ssymbol (list "qwe" '(quote qwe) 'qwe))

(cl-defun iintern (symbol-name)
  (if (stringp symbol-name)
    (if (empty-string-p symbol-name)
      nil
      (if (integer-string-p symbol-name)
	(integer-to-string symbol-name)
	(intern symbol-name)))
    symbol-name))
;;(mapcar #'symbolp (mapcar #'iintern (list 1 "a" 'b "")))

(cl-defun decolonize-symbol-name (symbol-name &optional string-key)
  (let ((res (if (= (char symbol-name 0) ?:)
	       (substring symbol-name 1) symbol-name)))
    (if string-key
      (funcall string-key res)
      res)))
;;(decolonize-symbol-name ":a" #'upcase)
;;(mapcar #'decolonize-symbol-name (list ":a" "a"))

(cl-defun decolonize-symbol (symbol &optional string-key)
  (ssymbol (decolonize-symbol-name (symbol-name symbol) string-key)))
;;(mapcar #'decolonize-symbol '(a :b))

(cl-defun listify (x)
  "Coerces x to become a cons. Also a lambda expression, which
also is a cons, will be encapsulated in a list."
  (if (or (not (listp x)) (functionp x))
    (list x) x))
;;(mapcar #'listify (list 'a (list 1 2 3) nil #'(lambda () nil)))


;;; symbols
(cl-defun concat-symbols (symbol &rest symbols)
  (if symbols
    (apply #'concat-symbols 
	   (iintern (concat (sstring symbol) (sstring (first symbols))))
	   (rest symbols))
    symbol))
;;(concat-symbols 'a 1)

(cl-defun symbol-left (symbol N)
  "Abbrevates SYMBOL to its N leftmost characters"
  (intern (substring (symbol-name symbol) 0 N)))
;;(symbol-left 'testtest 2)

(cl-defun symbol< (x y)
  "Apply `string<' on the symbol names of two symbols X Y."
  (string< (sstring x) (sstring y)))
;;(symbol< 'a 'b)

(cl-defun keyword-name (keyword-symbol)
  (if (keywordp keyword-symbol)
    (upcase (substring (symbol-name keyword-symbol) 1))
    keyword-symbol))
;;(keyword-name :key)

(cl-defun make-keyword (name)
  (intern (format ":%s" name)))
;;(make-keyword "qwe")

(cl-defun numcond-transform-clauses (clauses gx gy)
  "TODO: handle t and otherwise like in `cond'"
  `(cond ,@(mapcar #'(lambda (x) `((,(first x) ,gx ,gy) ,@(rest x)))
		       clauses)))
;;(pp (macroexpand '(numcond (3 3) ((= 'eq)))))

(cl-defmacro numcond (arg &rest clauses)
  (when (atom arg)
    (setf arg (list arg 0)))
  (with-gensyms (gx gy) 
    `(cl-destructuring-bind (,gx ,gy) (list ,@arg)
        ,(numcond-transform-clauses clauses gx gy))))
(cl-indent 'numcond 'case)
;;(numcond (4 3) (= 'eq) (< 'lt) (> 'geq))

(cl-defun call-if (predicate function x)
  "Return (FUNCTION X) if predicate is not nil.
Otherwise return x"
  (if predicate
    (funcall function x)
    x))

;;; PLIST utils
(cl-defun plist-position (plist prop)
  "Return the property position of PROP in PLIST.
The position includes both property symbol and value."
  (cl-loop for pos from 0
	for (property value) = (pop-list plist 2)
	if (eq property prop) return pos
	while plist))
;;(plist-position '(:qwe qwe :ewq ewq) :ewq)

(cl-defun plist-delete (plist prop)
  "Delete property PROP from PLIST. Destructive."
  (let ((pos (plist-position plist prop)))
    (and pos
	 (if (zerop pos)
	   (cddr plist)
	   (let ((last (nthcdr (1- (* 2 pos)) plist)))
	     (setf (cdr last) (cdddr last))
	     plist)))))
;;(plist-delete '(:qwe qwe :ewq ewq) :ewq)

(cl-defun plist-remove (plist prop)
  "Remove property PROP from PLIST."
  (plist-delete (copy-list plist) prop))
;;(plist-remove '(:qwe qwe :ewq ewq) :ewq)

(defmacro plist-pop (plist prop)
  "Pops property PROP from PLIST and return its value.
Return nil if PROP does not exist."
  `(awhen (plist-get ,plist ,prop)
     (setf ,plist (plist-delete ,plist ,prop))
     it))

(cl-defun modify-if (value test new-value)
  "Return VALUE if (TEST VALUE) evaluates to nil, otherwise
return NEW-VALUE"
  (if (funcall test value) new-value value))

(defmacro dprint (sexp &optional tag)
  `(if ,tag
     (message "%s: %s: %S" (iso-time) (sstring ,tag) ,sexp)
     (message "%s: %S" (iso-time) ,sexp)))
;;(dprint 123 'qwe)

(provide 'mb-utils-div)
