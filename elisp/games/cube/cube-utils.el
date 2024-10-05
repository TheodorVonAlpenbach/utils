(cl-defun cube-revert-string (s)
  (if (and (> (length s) 1)
	   (eql (char s 1) ?w))
    (remove-nth 1 s)
    (insert-sequence s "w" :start1 1)))

(cl-defun cube-revert-symbol (symbol)
  (intern (cube-revert-string (sstring symbol))))
;;(mapcar #'cube-revert-symbol '(U Uw U2 Uw2))

(cl-defun cube-revert (sexp)
  (when sexp
    (if (symbolp sexp)
     (cube-revert-symbol sexp)
     (reverse (mapcar #'cube-revert sexp)))))
;;(cube-revert '((U R Uw Rw U2) (U R Uw Rw U2)))

(cl-defun cube-rotate-symbol-y (symbol)
  (case symbol
    (r f) (R F) (f l) (F L) (l b) (L B) (b r) (B R) (M s) (s m) (m S) (S M)
    (otherwise symbol)))

(cl-defun cube-rotate-symbol-x (symbol)
  (case symbol
    (u f) (U F) (f d) (F D) (d b) (D B) (b u) (B U) (E S) (S e) (e s) (s E)
    (otherwise symbol)))

(cl-defun cube-rotate-symbol-z (symbol)
  (case symbol
    (u l) (U L) (l d) (L D) (d r) (D R) (r u) (R U) (M E) (E m) (m e) (e M)
    (otherwise symbol)))

(cl-defun cube-rotate-symbol (symbol &optional (n 1) (dir :y))
  "Rotate SYMBOL clockwise, e.g. F -> R, R2 -> B2, Mw -> Sw.
Only DIR :Y is currently supported"
  (if (/=  n 1)
    (cl-loop repeat n do (setf symbol (cube-rotate-symbol symbol 1 dir)))
    (let ((sym1 (intern (char (sstring symbol) 0))))
      (case dir
	(:y (cube-rotate-symbol-y sym1))
	(:x (cube-rotate-symbol-x sym1))
	(:z (cube-rotate-symbol-z sym1))))))

(cl-defun cube-fliplr-symbol (symbol)
  (intern
   (let ((s (sstring symbol)))
     (case (char s 0)
       (?l (cube-revert-string (cl-replace s "r" :end1 1)))
       (?r (cube-revert-string (cl-replace s "l" :end1 1)))
       (?L (cube-revert-string (cl-replace s "R" :end1 1)))
       (?R (cube-revert-string (cl-replace s "L" :end1 1)))
       (?M s)
       ((?U ?u ?D ?d ?F ?f ?B ?b ?y) (sstring (cube-revert-symbol symbol)))
       (error "Cannot handle symbol %S" symbol)))))
;;(mapcar #'cube-fliplr-symbol '(l rw L2 Rw2))
;;(cl-replace "abcdef" "ABCDEF" :start2 0 :end2 2)
;;(insert-sequence "l" "rw" :start1 0 :end1 1)

(cl-defun cube-fliplr (sexp)
  (when sexp
    (cond ((symbolp sexp) (cube-fliplr-symbol sexp))
	  ((integerp sexp) sexp)
	  (t (mapcar #'cube-fliplr sexp)))))
;;(cube-fliplr 'lw)
;;(cube-fliplr '((U R Uw Rw U2)2 (U R Uw Rw U2)))

(cl-defun cube-parse-algorithm (s &optional (reverse-char ?'))
  (cl-loop while (or (null i) (< i (length s)))
	for (o . i) = (read-from-string (cl-substitute ?w reverse-char s) i)
	collect o))

(cl-defun cube-sexp-to-string (sexp &optional (reverse-char ?'))
  (replace-regexp-in-string
   " 2" "2"
   (substring (cl-substitute reverse-char ?w (format "%S" sexp)) 1 -1)))
;;(cube-sexp-to-string '((R U Rw Uw)2))

(cl-defun cube-with-algorithm (fn algorithm &optional (reverse-char ?'))
  (cube-sexp-to-string (funcall fn (cube-parse-algorithm algorithm))))
;;(cube-with-algorithm #'cube-fliplr "l' U' l (L' U' L U) l' U l")
;;(cube-with-algorithm #'cube-fliplr "(r U2 R' U') (R U' r')")
;;(cube-sexp-to-string (cube-fliplr-sexp (cube-parse-algorithm "(r U2 R' U') (R U' r')")))
;;output-template

(provide 'cube-utils)
