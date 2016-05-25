(defvar *maths-categories* nil
  "List of arguement categories. Each category is a list of three
  elements: \(CATEGORY-SYMBOL DRAW-FN TEST-FN\), where
  CATEGORY-SYMBOL is a category symbol for the given category and
  it should be different from all other category symbols. DRAW-FN
  is a lambda expression for a function that draws a random set
  of arguments within this category. TEST-FN is a lambda
  expression that checkes if a set of arguments is within
  category or not. Hence, \(apply TEST-FN \(funcall DRAW-FN\)\)
  should always return non-nil.")

(cl-defmacro defcategory (name args &optional (doc ""))
  "TODO: create lambda functions only and store them in a global variable"
  ;;gensym these later
  (let ((draw-defun-name (intern (format "draw-%s" name)))
	(typep-defun-name (intern (format "%s-p" name))))
    `(progn
       (defun ,typep-defun-name (,(car (first args)) ,(car (second args)))
	 (and (cl-typep ,(car (first args)) (list 'integer ,@(cdadr (first args))))
	      (cl-typep ,(car (second args)) (list 'integer ,@(cdadr (second args))))))

       (defun ,draw-defun-name ()
	 (let* ((,(car (first args)) (random-integer ,@(cdadr (first args))))
		(,(car (second args)) (random-integer ,@(cdadr (second args)))))
	   (list ,(car (first args)) ,(car (second args))))))))
;;(defcategory ewq1 ((a (integer 0 9)) (b (integer 0 (- 9 a)))))
;;(apply #'ewq1-p (draw-ewq1))
;;(cl-find 'ewq1 *maths-categories* :key #'first)

(cl-defmacro defcategory (name args &key (reflect nil) (documentation ""))
  "TODO: create lambda functions only and store them in a global variable"
  ;;gensym these later
  (let ((draw-defun-name (intern (format "draw-%s" name)))
	(typep-defun-name (intern (format "%s-p" name))))
    `(progn
       (setf *maths-categories* (cl-remove ',name *maths-categories* :key #'first))
       (push (list
	      ',name
	      (lambda (,(car (first args)) ,(car (second args)))
		(and (cl-typep ,(car (first args)) (list 'integer ,@(cdadr (first args))))
		     (cl-typep ,(car (second args)) (list 'integer ,@(cdadr (second args))))))
	      
	      (lambda ()
		(let* ((,(car (first args)) (random-integer ,@(cdadr (first args))))
		       (,(car (second args)) (random-integer ,@(cdadr (second args)))))
		  (list ,(car (first args)) ,(car (second args)))))
	      :reflection reflection
	      :documentation documentation)
	     *maths-categories*))))

(defun draw-category (category-symbol)
  (funcall (third (assoc category-symbol *maths-categories*))))
;;(draw-category 'ewq1)

(defun category-p (category-symbol args)
  (apply (second (assoc category-symbol *maths-categories*)) args))
;;(category-p 'ewq1 '(2 4))

(defun category (args)
  (find-if (bind #'category-p args) *maths-categories* :key #'first))
;;(category '(9 0))

(defcategory +1+1+1 ((a (integer 0 9)) (b (integer 0 (- 9 a))))
  "Two one-digit non-negative integers that add to another one-digit
non-negative integer")

(defcategory +1+1+2 ((a (integer 0 9)) (b (integer (- 10 a) 9)))
  "Two one-digit non-negative integers that adds to a two-digit
positive integer")

(defcategory +1+n ((a (integer 0 9)) (b (integer 10)))
  "Two one-digit non-negative integers that adds to a two-digit
positive integer")

(typep 10 (list 'integer 10 1000))
