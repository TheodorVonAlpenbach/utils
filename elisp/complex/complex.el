;;;; complex.el -- the poor man's implementation of complex numbers

(defconst complex-operators
  '(+ - * / expt abs sqrt))

(cl-defun non-complex-symbol-name (symbol)
  (concat "non-complex-" (symbol-name symbol)))

(cl-defun non-complex-symbol (symbol &optional intern-hard)
  (funcall (if intern-hard #'intern #'intern-soft) 
	   (non-complex-symbol-name symbol)))
;;(non-complex-symbol '+ nil)

(cl-defun complex-symbol (symbol &optional intern-hard)
  (intern-soft (concat "complex-" (symbol-name symbol))))
;;(complex-symbol #'+)

(cl-defun init-non-complex-operators (&optional (operators complex-operators))
  (cl-loop for op in operators
	do (fset (non-complex-symbol op t) (symbol-function op))))

(cl-defun admin-non-complex-operators (action &optional (operators complex-operators))
  "ACTION is one of :init :redefine :reset"
  (cl-case action
    (:redefine (cl-loop for op in operators
		     do (fset op (symbol-function (complex-symbol op)))))
    (:reset (cl-loop for op in operators
		     do (fset op (symbol-function (non-complex-symbol op)))))))
;(init-non-complex-operators)
;;(admin-non-complex-operators :reset)
;;(mapcar #'symbol-function '(+ non-complex-+ complex-+))
;;(eql (symbol-function 'non-complex-+) (symbol-function '+))

;;(complex-sqrt -1)
;(with-complex (with-non-complex (+ 1 1)))
;;(with-complex (+ 1 1))
;;(pp (macroexpand '(with-complex (abs '(1 1)))))
;;(with-complex (abs '(1 1)))

;;; complex closure
(cl-defun with-complex-p ()
  (nequal (symbol-function '+) (symbol-function 'non-complex-+)))
;;(with-complex-p)

(defmacro with-complex (&rest body)
  "TODO: remove debug info"
  `(if (with-complex-p)
     (progn ,@body)
     (unwind-protect 
	  (progn 
;;	    (message "redefines")
	    (admin-non-complex-operators :redefine) 
;;	    (message "done")
	    ,@body)
;;	  (message "resets")
	  (admin-non-complex-operators :reset)
;;	  (message "done")
	  )))
(cl-indent 'with-complex 0)

(defmacro with-non-complex (&rest body)
  "TODO: remove debug info"
  `(if (not (with-complex-p))
     (progn ,@body)
     (unwind-protect 
       (progn
;;	 (message "resets-none")
	 (admin-non-complex-operators :reset)
;;	 (message "done-none")
	 ,@body)
;;       (message "redefines-none")
       (admin-non-complex-operators :redefine)
;;       (message "done-none")
       )))
(cl-indent 'with-non-complex 0)
;;(macroexpand '(with-complex (* '(1 1)) (apply #'* '(1 2))))
;;(with-complex (apply #'* '((1 1) (1 1))))

(defalias 'complex-re #'first)
(defalias 'complex-im #'second)

(cl-defun complex (x &optional (y 0)) (list x y))

(cl-defun complex-p (c)
  (and (listp c) (= (length c) 2) (cl-every #'numberp c)))
;;(mapcar #'complex-p '((1 2) 1 (1) (0 2)))

(cl-defun complex-ify (x &optional signal-on-error)
  (cond ((complex-p x) x)
	((numberp x) (complex x))
	(t (when signal-on-error (error "Argument %S is neither a number nor a complex" x)))))
;;(mapcar #'complex-ify '((1 2) 1 (1) (0 2)))

(cl-defun complex-clean (c)
  (if (zerop (complex-im c))
    (complex-re c) c))
;;(mapcar #'complex-clean '((1 2)(1 0)(0 2)))

(cl-defun complex-add (c1 c2)
  (complex-clean (cl-mapcar #'non-complex-+ (complex-ify c1) (complex-ify c2))))
;;(cl-mapcar #'complex-add '((1 2)(1 0)(0 2)) '((1 2)(1 0)(0 2)))

(cl-defun complex-sum (complex-numbers)
  (cl-reduce #'complex-add complex-numbers :initial-value '(0 0)))

(cl-defun complex-+ (&rest complex-numbers)
  (complex-sum complex-numbers))
;;(complex-+ '(1 2) '(1 0) '(0 2))

(cl-defun complex-negate (c) 
  (complex-clean (mapcar #'non-complex-- (complex-ify c))))
;;(complex-negate '(1 1))

(cl-defun complex-- (complex-number &rest complex-numbers)
  (if complex-numbers
    (complex-add complex-number 
		 (complex-negate (complex-sum complex-numbers)))
    (complex-negate complex-number)))
;;(complex-- '(1 2) '(1 0) '(0 2) '(123 123))

(cl-defun complex-multiply (c1 c2)
  (cl-destructuring-bind ((a b) (c d)) 
      (list (complex-ify c1) (complex-ify c2))
    (complex (non-complex-- (non-complex-* a c) 
			    (non-complex-* b d))
	     (non-complex-+ (non-complex-* b c) 
			    (non-complex-* a d)))))
;;(cl-mapcar #'complex-multiply '((1 2)(1 0)(0 2)) '((1 2)(1 0)(0 2)))

(cl-defun complex-product (complex-numbers)
  (cl-reduce #'complex-multiply complex-numbers :initial-value 1))

(cl-defun complex-* (&rest complex-numbers)
  (complex-product complex-numbers))
;;(complex-* '(1 2) '(1 0) '(0 2))

(cl-defun complex-inverse (c) 
  (cl-destructuring-bind (c d) (complex-ify c)
    (with-non-complex
	(let ((denominator (float (+ (sq c) (sq d)))))
	  (complex (/ c denominator)(/ (- d) denominator))))))
;;(complex-inverse '(1 1))

(cl-defun complex-/ (complex-number &rest complex-numbers)
  (complex-multiply complex-number
		    (complex-inverse (complex-product complex-numbers))))
;;(complex-/ '(1 2) '(1 0) '(0 2) '(123 123))

;;; polar
(cl-defun complex-abs-squared (x)
  (with-non-complex
    (+ (sq (complex-re x)) (sq (complex-im x)))))

(cl-defun complex-abs (x)
  (with-non-complex
    (if (complex-p x)
      (sqrt (complex-abs-squared x))
      (abs x))))
;;(complex-abs '(1 1))
;;(with-complex (abs '(1 1)))
;;(init-non-complex-operators)

(cl-defun complex-argument (c)
  (with-non-complex 
    (atan2 (complex-re c) (complex-im c))))
;;(complex-argument '(-1 0))

(cl-defun complex-polar (x &optional reverse)
  (if reverse
    (with-non-complex
      (complex (* (first x) (cos (second x))) 
	       (* (first x) (sin (second x)))))
    (list (complex-abs x) (complex-argument x))))
;;(complex-polar (complex-polar '(1 1)) t)

(defmacro complex-as-polar (x &rest body)
  "Evaluate body as if complex number X is in polar form.
This is just confusing. Just write this damned code every time,
cf. `complex-expt'"
  `(let ((,x (complex-polar ,x)))
     (complex-polar (progn ,@body) t)))
(cl-indent 'complex-as-polar 'case)

;;; derived functions
(cl-defun complex-expt (arg1 arg2)
  (cl-destructuring-bind (r arg) (complex-polar arg1)
    (complex-polar (with-non-complex 
		     (list (expt r arg2) (* arg arg2)))
		   t)))
;;(complex-expt '(-1 0) .5)

(cl-defun complex-sqrt (arg)
  (complex-expt (complex-ify arg) 0.5))
;;(complex-sqrt -1)
;;(with-complex (sqrt -1))

;;; print
(cl-defun complex-to-string (c)
  (apply #'format "%s+i%s" c))

(provide 'complex)
