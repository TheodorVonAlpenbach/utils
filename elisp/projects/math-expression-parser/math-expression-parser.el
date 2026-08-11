(defconst +operators+
  "List of allowed operators."
  (""))

(defun math-expression-parse-tokens (tokens)
  "Convert a math expression string tokens to a sexp"
  )

(defun math-expression-tokenizer (string)
  "Convert a math expression string to tokens"
  (split-string string))
;;(math-expression-tokenizer "-1 + (2 - 1)") =>
;; '((operator - '(unary binary))
;;   (number 1)
;;   (operator + binary)
;;   (delimiter \( start)
;;   (number 2)
;;   (operator - binary)
;;   (number 1)
;;   (delimiter \) start))


(defun math-expression-parser (string)
  "Convert a math expression string to a sexp

Examples:
\"1\" -> 1
\"log 3\", \"log(3)\" -> (log 3) 
\"2 + 2\" -> (+ 2 2)
"
  (mparse-list (mtokenizer string)))

(defun mparse-list (tokens)
  "Make sure to read to next end delimiter"
  (if (mright-delimiter-p (car tokens))
    (list () (cdr tokens))
    (cl-destructuring-bind (expr rtokens) (mparse-expr (rest tokens))
      (if (mright-delimiter-p (car rtokens))
	(list () (cdr rtokens))
	(if (eq (car rtokens) ",")
	  (cl-destructuring-bind (exprs rtokens) (mparse-list (cdr rtokens))
	    (list (cons expr exprs) rtokens))
	  (error "unexpected end of list segment"))))))
;;(mparse-expr '(("(") (function "log" 1) (number "2")(")")))

(defun mparse-expr (tokens)
  "Make sure to read to next end delimiter"
  (if (not tokens)
    (list () tokens)
    (let ((t0 (car tokens)))
      (cond
	((mleft-delimiter-p t0)
	 (mparse-list (cdr tokens)))
	((mnumber-p t0)
	 (let ((lhsop (second tokens)))
	   (if (not lhsop)
	     (list t0 ())
	     (unless (lhs-operator-p lhsop)
	       (error "Isolated lhs number must be followed by a lhs operator"))
	     (if (rhs-operator-p lhsop)
	       (cl-destructuring-bind (expr rtokens) (mparse-expr (nthcdr 2 tokens))
		 (list (mcorrect-precedence (list lhsop t0 expr)) rtokens))
	       (list (list lhsop expr) rtokens)))))
	((rhs-operator-p t0)
	 (mparse-expr (cdr tokens)))
	((mfunction-p t0)
	 (cl-destructuring-bind (expr rtokens) (mparse-expr (cdr tokens))
	   (list (list t0 expr) rtokens))
	 (mparse-expr (cdr tokens)))
	(t
	 (error "Unknown token %S" t0))))))
;;(mparse-expr '(("(") (function "log" 1) (number "2")))
;;(mparse-expr '((number "1") (operator "+" binary) (number "2")))

(defun mcorrect-precedence (expr)
  expr)

(defun mleft-delimiter-p (token) (equal (car token) "("))
(defun mright-delimiter-p (token) (equal (car token) ")"))

(defun mfunction-p (token) (eq (car token) 'function))

(defun mnumber-p (token) (eq (car token) 'number))
;;(mnumber-p '(number "1"))

(defun binary-operator-p (token)
  (and (eq (car token) 'operator)
       (eq (third token) 'binary)))

(defun lhs-operator-strict-p (token)
  (and (eq (car token) 'operator)
       (eq (third token) 'lhs)))

(defun lhs-operator-p (token)
  (or (binary-operator-p token)
      (lhs-operator-strict-p token)))

(defun rhs-operator-strict-p (token)
  (and (eq (car token) 'operator)
       (eq (third token) 'rhs)))

(defun rhs-operator-p (token)
  (or (binary-operator-p token)
      (rhs-operator-strict-p token)))
;;(mnumber-p '(number "1"))

;;(cadr '(0 1 2 3))
;;(nthcdr 2 '(0 1 2 3))

(provide 'math-expression-parser)
