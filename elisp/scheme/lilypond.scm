(define ha-modifier-regexp "([+-|])")
(define ha-adder-regexp (format #f "((~a)~a)" integer-regexp ha-modifier-regexp))
(define ha-adders-regexp (format #f "(~a(,~a)*)" ha-adder-regexp ha-adder-regexp))
(define ha-base-function-regexp "([TDS])")
(define ha-base-function-modifier-regexp "([sm])")
(define ha-function-regexp (format #f "(~a~a)" ha-base-function-regexp ha-base-function-modifier-regexp))
(define ha-regexp (format #f "~a~a?" ha-function-regexp ha-adders-regexp))
;;(define qwe (string-match ha-regexp "Ts7+,5-"))
;;(string-match "(?:a)" "a")

(define (ha-function-parse match)
  (let ((base-function (match:substring match 2))
	(base-function-modifier (match:substring match 3)))
    (list base-function base-function-modifier)))

(define (ha-adders-parse match)
  (let ((base-function (match:substring match 2))
	(base-function-modifier (match:substring match 3)))
    (list base-function base-function-modifier)))

(define (ha-parse string)
  (let ((match (string-match ha-regexp "Ts7,5")))
    (list (ha-function-parse match)
	  (ha-adders-parse match))))

(define (match:start-end match n)
  (cons (match:start match n) (match:end match n)))

(define (match->list match)
  (let ((res (list)))
    (do ((i 0 (1+ i)))
	((>= i (match:count match)))
      (set! res (cons (match:start-end match i) res)))
    (reverse res)))

(define (match-list->tree match-list)
  (and match-list
       (member-if match-list )))

(define (match->tree match)
  )


