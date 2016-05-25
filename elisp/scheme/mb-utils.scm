;;macros
(use-syntax (ice-9 syncase))
(define-syntax when
       (syntax-rules ()
         ((when condition exp ...)
          (if condition
              (begin exp ...)))))
;;(when 1 1 2 3)

(define (list:delete-if! list pred)
  "Extend with keywords?"
  (if (null? list)
      list
      (if (pred (car list))
	  (list:delete-if! (cdr list) pred)
	  (cons (car list)
		(list:delete-if! (cdr list) pred)))))
;;(list:delete-if! '(1 2 3 4) #'(lambda (x) (odd? x)))

(define (nbutlast list n)
  (list-cdr-set! list (- (length list) 2) '())
  list)
;;(nbutlast '(a b c d) 1)

(define (last-element list)
  (car (last-pair list)))
;;(last-element '(a b c))

(define (list:member-if list pred)
  "Extend with keywords?"
  (if (null? list) 
      list
      (if (pred (car list))
	  list
	  (list:member-if (cdr list) pred))))
;;(list:member-if '(1 3 5 2 1) #'even?)


;;harmony analysis
(define integer-regexp "0|[1-9][0-9]*")

;;loop
(define-syntax loop
  (syntax-rules (for below collect)
    ((_ for <x> below <value> collect <expression>)
     (do ((<x> 0 (1+ <x>))
	  (res '() (cons <expression> res)))
	 ((>= <x> <value>)
	  (reverse! res))))
    ((_ for <x> in <list> collect <expression>)
     (let ((<x> #f))
	 (do ((_list <list> (cdr _list))
	  (res '() (cons <expression> res)))
	 ((null? _list)
	  (reverse! res))
       (set! <x> (car _list)))))))
;;(loop for x below 10 collect x)
;;(loop for x in '(a b c) collect x)

	 
;; modifies both let assignments and let recursive utility
(define-syntax let*
  (syntax-rules ()
    ((_ (ini) exp ...)
     (let (ini)
       exp ...))
    ((_ (ini more ...) exp ...)
     (let (ini)
       (let* (more ...) exp ...)))

    ((_ fn ((a b) ...) exp ...)
     (let* ((a b) ...)
       (let fn ((a a) ...)
	 exp ...)))))
;;(let* ((x 1) (y (+ 1 x))) (list x y))
;;(let* lp ((x 1) (y (+ 1 x))) (if (> x 5) (list x y) (lp (1+ x) (1+ y))))


;; helper for do*
(define-syntax build-do-return
  (syntax-rules ()
    ((_ exp)
     exp)
    ((_ last exp)
     (if last exp #f))
    ((_ test more ...)
     (if test
	 (last-element (list more ...))
	 (build-do-return more ...)))))
;;(let* ((i 0) (j i)) (build-do-return (> i 100) (> j 2) i))

;; same as do but with let* assignments
(define-syntax do*
  (syntax-rules ()
    ((do* ((sym val incr) ...) (test ...) exp ...)
     (let* lp ((sym val) ...)
       (or (build-do-return test ...)
	   (begin exp ... (lp incr ...)))))))
;(do* ((i 0 (1+ i)) (j i (+ 2 j))) ((> i 100) (> j 10) i) (display (list i j)))

(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ test then else)
       (with-syntax ((it (datum->syntax-object x 'it)))
		    (syntax (let ((it test))
			      (if it then else)))))
      ((_ test then)
       (with-syntax ((it (datum->syntax-object x 'it)))
		    (syntax (let ((it test))
			      (if it then))))))))
;(aif (+ 1 1) (+ it 10))

