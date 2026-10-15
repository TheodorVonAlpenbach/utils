;;; test-mb-utils-math.el --- Tests for mb-utils-math -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-math)

(ert-deftest mb-utils-math-generate-addends-fixed ()
  "Test of `generate-addends-fixed'"
  (should (equal (generate-addends-fixed 577 51)
		 (cl-destructuring-bind (q r) (cl-floor 577 51)
		   (append (make-list r (1+ q))
			   (make-list (- 51 r) q))))))

(ert-deftest mb-utils-math-unsignum ()
  "Test of `unsignum'"
  (should (equal (mapcar #'unsignum '(-2 0 2)) '(1 0 -1)))
  (should (equal (unsignum 1 -1) 1)))

(ert-deftest mb-utils-math-uint-to-n-base ()
  "Test of `uint-to-n-base'"
  (should (equal (cl-loop for i in (a-b 99 101) collect (uint-to-n-base i))
		 '((9 9) (1 0 0) (1 0 1))))
  (should (equal (cl-loop for i in '(0 1 9 10) collect (uint-to-n-base i))
		 '(() (1) (9) (1 0))))
  (should (equal (cl-loop for i in '(0 1 9 10) collect (uint-to-n-base i 10 2))
		 '((0 0) (0 1) (0 9) (1 0))))
  (should (equal (cl-loop for i in '(0 1 2 3 4) collect (uint-to-n-base i 2))
		 '(() (1) (1 0) (1 1) (1 0 0))))
  (should (equal (cl-loop for i in '(0 1 2 3 4) collect (uint-to-n-base i 2 3))
		 '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0)))))

(ert-deftest mb-utils-math-uint-length ()
  "Test of `uint-length'"
  (should (equal (mapcar #'uint-length (0-n 11))
		 '(0 1 1 1 1 1 1 1 1 1 2)))
  (should (equal (uint-length (0-n 11)) 2)))

(ert-deftest mb-utils-math-cumsum ()
  "Test of `cumsum'"
 (should (equal (cumsum (0-n 3)) '(0 1 3)))
 (should (equal (cumsum (0-n 3) :initial-value 2) '(2 3 5)))
 (should (equal (cumsum (1-n 3) :key #'* :initial-value 1) '(1 2 6)))
 (should (equal (cumsum (cl-coerce (0-n 3) 'vector)) [0 1 3])))

(ert-deftest mb-utils-math-sum ()
  "Test of `sum'"
  (should (equal (sum '(1 2 3 4) :start 1 :initial-value 1) 10))
  (should (equal (sum '(1 2 3 4) :operator #'+ :start 1 :initial-value 1) 10))
  (should (equal (sum '(1 2 3 4) :operator #'* :start 1 :initial-value 1) 24)))

(ert-deftest mb-utils-math-floor-to ()
  "Test of `floor-to'"
  (should (equal (floor-to 70 25) 50))
  (should (equal (floor-to 75 25) 75)))

(ert-deftest mb-utils-math-ceiling-to ()
  "Test of `ceiling-to'"
  (should (equal (ceiling-to 70 25) 75))
  (should (equal (ceiling-to 75 25) 75)))

(ert-deftest mb-utils-math-next-greater-multiple ()
  "Test of `next-greater-multiple'"
  (should (equal (next-greater-multiple 4 5) 5))
  (should (equal (next-greater-multiple 5 5) 10))
  (should (equal (next-greater-multiple 6 5) 10)))

(ert-deftest mb-utils-math-next-smaller-multiple ()
  "Test of `next-smaller-multiple'"
  (should (equal (next-smaller-multiple 4 5) 0))
  (should (equal (next-smaller-multiple 5 5) 0))
  (should (equal (next-smaller-multiple 6 5) 5)))

(ert-deftest mb-utils-math-random-weighted-element ()
  "Test weighted selection without depending on random samples."
  (let ((elements '((a 1) (b 5) (c 10))))
    (cl-letf (((symbol-function 'random-float)
	       (lambda (&rest _args) 0)))
      (should (eq (random-weighted-element elements) 'a)))
    (cl-letf (((symbol-function 'random-float)
	       (lambda (&rest _args) 5.5)))
      (should (eq (random-weighted-element elements) 'b)))))

(ert-deftest mb-utils-math-area-secant ()
  "Test of `area-secant'"
  (should (equal (area-secant 0) 0.0))
  (should (equal (area-secant 1) 0.07926450759605175))
  (should (equal (area-secant float-pi) (/ float-pi 2))))

(ert-deftest mb-utils-math-num-digits ()
  "Test of `num-digits'"
  (should (= (num-digits 1) (num-digits 9) 1))
  (should (= (num-digits 10) (num-digits 99) 2))
  (should (= (num-digits 100) (num-digits 999) 3))
  (should (= (num-digits (expt 10 10)) (num-digits (1- (expt 10 11))) 11)))

(ert-deftest mb-utils-math-split-mod ()
  "Test of `split-mod'"
  (should (equal (split-mod 123 10) '(120 3))))

(ert-deftest mb-utils-math-byte-conversions ()
  (should (equal (int-to-n-bit-bytes #x123456 8 2) '(#x34 #x56)))
  (let ((bytes '(1 0)))
    (should (= (bytes-to-int bytes :byte-size 7 :endianness :big) 128))
    (should (equal bytes '(1 0))))
  (let ((string (bytes-to-string '(0 127 128 255))))
    (should-not (multibyte-string-p string))
    (should (equal (string-to-bytes string) '(0 127 128 255))))
  (should (= (int-to-signed-byte -128 8) 128))
  (should (= (int-to-signed-byte 127 8) 127))
  (should-error (int-to-signed-byte -129 8))
  (should-error (int-to-signed-byte 128 8))
  (should (= (2s-complement #x80 8) -128))
  (should (= (2s-complement #x7f 8) 127))
  (let ((convert (2s-complement-function 8)))
    (should (= (funcall convert #x80) -128))))

(ert-deftest mb-utils-math-geo-distance ()
  (should (< (abs (- (geo-distance '(0.0 0.0) '(60.0 90.0))
		     (* 6371000.0 (/ float-pi 2))))
	     1e-6)))

(ert-deftest mb-utils-math-interest-annual ()
  (should (= (interest-annual 1000 0.05 2) 1102.5)))

(ert-deftest mb-utils-math-cycle-badness ()
  (should (= (cycle-badness '(a b a b)) 0.0))
  (should (> (cycle-badness '(a a b b)) 0.0)))

(ert-deftest mb-utils-math-jstime ()
  (should (equal (jstime 0) '(1970 0 0 0 0 0)))
  (should (equal (jstime (* 365 24 60 60)) '(1971 0 0 0 0 0)))
  (should (equal (jstime 0 2000) '(2000 0 0 0 0 0)))
  (should (equal (jstime (* 60 60 24 59) 2000) '(2000 1 28 0 0 0))))

(ert-deftest mb-utils-math-unimplemented-functions-error ()
  (should-error (list-move 'a '(a) nil) :type 'error)
  (should-error (swimming-relative-distances) :type 'error))

(provide 'test-mb-utils-math)

;;; test-mb-utils-math.el ends here
