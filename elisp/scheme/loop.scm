;; this should be hidden within the module
(use-syntax (ice-9 syncase))

(define ret__ '())

(define-syntax bloop
  (syntax-rules (with = for from to by below collect sum maximize minimize append do and if when unless else do return initially finally)
    ;;building statements from stack: and
    ((_ ((elses x and y s ...) data))
     (bloop ((elses (begin y x) s ...) data)))

    ((_ ((() x if t s ...) data))
     (bloop ((() (if t x) s ...) data)))

    ((_ (((outer-else else* ...) x if t s ...) data))
     (bloop ((() (if t x outer-else) s ...) data)))

    ((_ (((else* ...) x else s ...) data))
     (bloop (((else* ... x) s ...) data)))

    ((_ ((() x finally s ...) (vars inits incs tests before (after ...) ret-val)))
     (bloop ((() s ...) (vars inits incs tests before (after ... x) ret-val))))

    ;;finish
    ((_ ((() s ...) (vars inits incs tests before after ret-val)))
     (build-loop vars inits incs tests (s ...) before after ret-val))

    ;;"globals"
    ((_ (stack ((var* ...) inits incs tests before after ret-val)) with <x> = <value> rest ...)
     (bloop (stack ((var* ... (<x> <value>))  inits incs tests before after ret-val)) rest ...))

    ;;; iterations
    ;;; NB too many possibilties this should be parsed as well using stack
    ;;; For now, I only hardcode the variants I need. Indeed, loop is powerful...
    ;;; Idea: 
    ;;; when 'for x rest ...' is encountered, set stack object to ((x 0) (1+ x) #f)
    ;;; then
    ;;; if 'from start' change to ((x start) inc test)
    ;;; if 'upfrom start' change to ((x start) inc test)
    ;;; if 'downfrom start' change to ((x start) (1- x) test)
    ;;; if 'to end' change to (init inc (> x end))
    ;;; if 'below end' change to (init inc (>= x end))
    ;;; if 'downto end' change to (init (1- x) (< x end))
    ;;; if 'above end' change to (init (1- x) (<= x end))
    ;;; if 'by increment' change to (init increment test)
    ;;; if else (with a non empty stack), tranfer stack content to data slots

    ;; for VAR from EXPR1 to EXPR2 by EXPR3
    ((_ (stack (vars (init* ...) (inc* ...) (test* ...) before after ret-val)) for x from <start> to <end> by <increment> rest ...)
     (bloop (stack (vars (init* ... (x <start>)) (inc* ... (+ x <increment>)) (test* ... (> x <end>)) before after ret-val)) rest ...))

    ;; for VAR below EXPR
    ((_ (stack (vars (init* ...) (inc* ...) (test* ...) before after ret-val)) for <x> below <value> rest ...)
     (bloop (stack (vars (init* ... (<x> 0)) (inc* ... (1+ <x>)) (test* ... (>= <x> <value>)) before after ret-val)) rest ...))

    ;;building stack: finally
    ((_ ((elses s ...) data) finally rest ...)
     (bloop ((elses finally s ...) data) rest ...))

    ;;building stack: collect
    ((_ ((() s ...) ((var* ...) inits incs tests before after ret-val)) collect x into y rest ...)
     (bloop ((() (set! y (append y (list x))) s ...) ((var* ... (y '())) inits incs tests before after ret-val)) rest ...))
    ((_ ((() s ...) (vars inits incs tests before after (ret-val* ...))) collect x rest ...)
     (bloop ((() (set! ret__ (cons x ret__)) s ...) (vars inits incs tests before after (ret-val* ... ((set! ret__ '()) (reverse! ret__))))) rest ...))

;;     ;;building stack: sum
;;     ((_ ((() s ...) ((var* ...) inits incs tests before after ret-val)) sum x into y rest ...)
;;      (bloop ((() (set! y (+ x y)) s ...) ((var* ... (y 0)) inits incs tests before after ret-val)) rest ...))
;;     ((_ ((() s ...) (vars inits incs tests before after (ret-val* ...))) sum x rest ...)
;;      (bloop ((() (set! ret__ (+ x ret__)) s ...) (vars inits incs tests before after (ret-val* ... ((set! ret__ 0) ret__)))) rest ...))

;;     ;;building stack: maximize
;;     ((_ ((() s ...) ((var* ...) inits incs tests before after ret-val)) maximize x into y rest ...)
;;      (bloop ((() (set! y (if y (max x y) x)) s ...) ((var* ... (y #f)) inits incs tests before after ret-val)) rest ...))
;;     ((_ ((() s ...) (vars inits incs tests before after (ret-val* ...))) maximize x rest ...)
;;      (bloop ((() (set! ret__ (if ret__ (max x ret__) x)) s ...) (vars inits incs tests before after (ret-val* ... ((set! ret__ #f) ret__)))) rest ...))

;;     ;;building stack: minimize
;;     ((_ ((() s ...) ((var* ...) inits incs tests before after ret-val)) minimize x into y rest ...)
;;      (bloop ((() (set! y (if y (min x y) x)) s ...) ((var* ... (y #f)) inits incs tests before after ret-val)) rest ...))
;;     ((_ ((() s ...) (vars inits incs tests before after (ret-val* ...))) minimize x rest ...)
;;      (bloop ((() (set! ret__ (if ret__ (min x ret__) x)) s ...) (vars inits incs tests before after (ret-val* ... ((set! ret__ #f) ret__)))) rest ...))

    ;;building stack: do
    ((_ ((() s ...) data) do x rest ...)
     (bloop ((() x s ...) data) rest ...))

    ;;building stack: and
    ((_ ((() s ...) data) and rest ...)
     (bloop ((() and s ...) data) rest ...))

    ;;building stack: if
    ((_ ((() s ...) data) if test rest ...)
     (bloop ((() if test s ...) data) rest ...))

    ;;building stack: when = if
    ((_ ((() s ...) data) when test rest ...)
     (bloop ((() if test s ...) data) rest ...))

    ;;building stack: unless = (not if)
    ((_ ((() s ...) data) unless test rest ...)
     (bloop ((() if (not test) s ...) data) rest ...))

    ;;building stack: else
    ((_ ((() s ...) data) else rest ...)
     (bloop ((() else s ...) data) rest ...))

    ;;building stack: return
    ((_ ((() s ...) data) return x rest ...)
     (bloop ((() (throw 'return x) s ...) data) rest ...))))

(define-macro (mloop . rest)
  `(first ,rest))


(define-syntax loop
  (syntax-rules ()
    ((loop rest ...)
     (bloop ((()) (() () () () () () ())) rest ...))))

(define-syntax build-loop
  (syntax-rules ()
    ((_ (var var* ...) init incr test exp before after ret-val)
     (let* (var var* ...)
       (build-loop () init incr test exp before after ret-val)))

    ((_ () init incr test exp before after ())
     (build-loop () init incr test exp before after ((#f (if #f #f)))))

    ((_ () (init ...) (incr ...) (test ...) (exp ...) (before ...) (after ...) ((ret-init ret-val) rest* ...))
     (catch 'return 
	    (lambda ()
	      (begin
		ret-init
		before ...
		(let* lp (init ...)
		      (if (not (or test ...))
			  (begin exp ...
				 (lp incr ...))))
		after ... 
		ret-val))
	    (lambda (c ret)
	      ret)))))
