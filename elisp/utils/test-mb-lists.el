;;; test-mb-lists.el --- Tests for mb-lists -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-lists)

(ert-deftest mb-lists-combine-1 ()
  "Test of `combine-1'"
  (should (equal (combine-1 nil) nil))
  (should-error (combine-1 1))
  (should (equal (combine-1 '(a)) '((a))))
  (should (equal (combine-1 '((a))) '((a))))
  (should (equal (combine-1 '(a b)) '((a b))))
  (should (equal (combine-1 '((a b) (c))) '((a c) (b c))))
  (should (equal (combine-1 '((a b) c)) '((a c) (b c))))
  (should (equal (combine-1 '((a b) (c d))) '((a c) (a d) (b c) (b d))))
  (should (equal (combine-1 '((a b) (c d e)))
			    '((a c) (a d) (a e) (b c) (b d) (b e))))
  (should (equal (combine-1 '((a b) (c d) (e)))
			    '((a c e) (a d e) (b c e) (b d e))))
  (should (equal (combine-1 '((a b) (c d) e))
			    '((a c e) (a d e) (b c e) (b d e))))
  (should (equal (combine-1 '((a b) e (c d)))
			    '((a e c) (a e d) (b e c) (b e d)))))

(ert-deftest mb-lists-combine ()
  "Test of `combine'"
  (should (equal (combine '()) nil))
  (should (equal (combine '(a)) '((a))))
  (should (equal (combine '((a))) '((a))))
  (should (equal (combine '(a b)) '((a b))))
  (should (equal (combine '((a b))) '((a b))))
  (should (equal (combine '(a (b c))) '((a b) (a c))))
  (should (equal (combine '((a b) c)) '((a c) (b c))))
  (should (equal (combine '((a d) (b c))) '((a b) (a c) (d b) (d c))))
  (should (equal (combine '((a) (b) (c))) '((a b c))))
  (should (equal (combine '(("a" "b") ("c") ("e" "f")))
		 '(("a" "c" "e") ("a" "c" "f") ("b" "c" "e") ("b" "c" "f"))))
  (should (equal (combine '(("a" "b") ("c") ("e" "f")) :key #'concat)
		 '("ace" "acf" "bce" "bcf"))))

(ert-deftest mb-lists-infix-list ()
  "Test of `infix-list'"
  (should (equal (infix-list '(a b c) #'1+ t)
		 '(a 1 b 2 c)))
  (should (equal (infix-list '(a b c)
			     #'(lambda (i n) (if (= i (- n 2)) 'and '\,)) t)
		 '(a \, b and c))))

(ert-deftest mb-lists-repeat-elements ()
  "Test of `repeat-elements'"
  (should (equal (repeat-elements (0-n 3) 3) '(0 0 0 1 1 1 2 2 2)))
  (should (equal (repeat-elements (0-n 3)) '(0 0 1 1 2 2)))
  (should (equal (repeat-elements (0-n 3) 2) '(0 0 1 1 2 2)))
  (should (equal (repeat-elements (0-n 3) 1) '(0 1 2)))
  (should (equal (repeat-elements (0-n 3) 0) nil)))

(ert-deftest mb-lists-zip ()
  "Test of `zip'"
  (should (equal (zip nil) nil))
  (should (equal (zip '(0 2 4) '(1 3 3)) '(0 1 2 3 4 3)))
  (should (equal (butlast (zip '(1 2 3) 'og)) '(1 og 2 og 3))))

(ert-deftest mb-lists-transpose ()
  "Test of `transpose'"
  (should (equal (transpose nil) nil))
  (should (equal (transpose '((a b c) (d e f))) '((a d) (b e) (c f)))))

(ert-deftest mb-lists-rotatef-list ()
  "Test of `rotatef-list'"
  (let ((l '(a b c)))
    (should (equal (rotatef-list l) '(b c a)))
    (should (equal l '(b c a)))
    (rotatef-list l 1)
    (should (equal l '(c a b)))
    (rotatef-list l 2)
    (should (equal l '(b c a)))))

(ert-deftest mb-lists-swap ()
  "Test of `gen-swap'"
  (should (equal (swap 'a '((a b) (c d))) 'b))
  (should (equal (swap 'b '((a b) (c d))) 'a))
  (should (equal (swap 'c '((a b) (c d))) 'd))
  (should (equal (swap 'd '((a b) (c d))) 'c))
  (should-not (equal (swap "a" '(("a" "b") ("c" "d"))) "b"))
  (should (equal (swap "a" '(("a" "b") ("c" "d")) :test #'string=) "b")))

(ert-deftest mb-lists-group ()
  "Test of `group'"
  (should (equal (group '(a b d a d a b)
		   :test (lambda (_x y) (eql y 'd)))
		 '((a) (b d) (a d) (a) (b)))))

(ert-deftest mb-lists-equivalence-class ()
  "Test of `equivalence-class"
  (should (equal (equivalence-class '(a b d a d a b)) '((a a a) (b b) (d d))))
  (should (equal (equivalence-class '((a) (b) (c) (a) (b) (a) (d)) :key #'car)
		 '(((a) (a) (a)) ((b) (b)) ((c)) ((d))))))

(ert-deftest mb-lists-head ()
  "Test of `head'"
 (should (equal (head 0 '(a b c)) nil))
 (should (equal (head 1 '(a b c)) '(a))))

(ert-deftest mb-lists-accumulate-list ()
  "Test of `accumulate-list'"
  (should (equal (accumulate-list
                  '(a b c a b a d)
                  :test (lambda (a b)
                          (string< (symbol-name a) (symbol-name b))))
		 '((a 3) (b 2) (c 1) (d 1))))
  (should (equal (accumulate-list '("a" "b" "c" "a" "b" "a" "d") :test #'string<)
		 '(("a" 3) ("b" 2) ("c" 1) ("d" 1)))))

(ert-deftest mb-lists-n-0 ()
  "Test of `n-0'"
  (should (equal (n-0 3) '(2 1 0))))

(ert-deftest mb-lists-equivalence-class-with-key ()
  "Test of `equivalence-class-with-key'"
 (should (equal (equivalence-class-with-key '("a" "ab" "abc" "b" "bc" "bcd")
					    :key #'length)
		'((1 ("b" "a")) (2 ("bc" "ab")) (3 ("bcd" "abc")))))
 (should (equal (equivalence-class-with-key '("a" "ab" "abc" "b" "bc" "bcd")
					    :key #'(lambda (x) (substring x 0 1))
					    :test #'equal)
		'(("a" ("abc" "ab" "a")) ("b" ("bcd" "bc" "b"))))))

(ert-deftest mb-lists-listify-atoms ()
  "Test of `listify-atoms'"
  (should (equal (listify-atoms '(a (1 2 3) (d f))) '((a a a) (1 2 3) (d f)))))

(ert-deftest mb-lists-memcase ()
  "Test of `memcase'"
  (should (equal (memcase '(a b c) (a 'A) (otherwise 'B)) 'A))
  (should (equal (memcase '(a b c) (d 'A) (otherwise 'B)) 'B))
  (should (equal (memcase '(a b c) ((a d) 'A) (otherwise 'B)) 'A))
  (should (equal (memcase '(a b c) ((d e) 'A) (otherwise 'B)) 'B)))

(ert-deftest mb-lists-headtail ()
  "Test of `headtail'"
  (should (equal (headtail nil 17 56) nil))
  (should (equal (headtail (0-n 4)) '(0 3)))
  (should (equal (headtail (0-n 4) 1) '(0 3)))
  (should (equal (headtail (0-n 4) 1 0) '(0)))
  (should (equal (headtail (0-n 4) 0 1) '(3)))
  (should (equal (headtail (0-n 4) 2) '(0 1 2 3)))
  (should (equal (headtail (0-n 4) 2 2) '(0 1 2 3)))
  (should (equal (headtail (0-n 4) 2 0) '(0 1)))
  (should (equal (headtail (0-n 4) 2 1) '(0 1 3)))
  (should (equal (headtail (0-n 4) 0 2) '(2 3)))
  (should (equal (headtail (0-n 4) 1 2) '(0 2 3)))
  (should (equal (headtail (0-n 4) 3) '(0 1 2 3)))
  (should (equal (headtail (0-n 4) 1 1 '(foo bar)) '(0 foo bar 3))))

(ert-deftest mb-lists-cars ()
  "Test of `cars'"
  (should (equal (cars '((a b c) (d e))) '(a d))))

(ert-deftest mb-lists-a-b ()
  "Test of `a-b'"
  (should (equal (a-b 2 7) '(2 3 4 5 6 7)))
  (should (equal (a-b -1 2 2) '(-1 1)))
  (should (equal (a-b 2 7 1 #'1+) '(3 4 5 6 7 8))))

(ert-deftest mb-lists-0-n ()
  "Test of `0-n'"
  (should (equal (0-n 3) '(0 1 2)))
  (should (equal (0-n 5 2) '(0 2 4)))
  (should (equal (0-n 3 1 (lambda (x) (* x x))) '(0 1 4))))

(ert-deftest mb-lists-pad-list ()
  "Test of `pad-list'"
  (should (equal (pad-list '(a b c) 5 'd t) '(a b c d d)))
  (should (equal (pad-list '(a b c) 5 'd) '(d d a b c))))

(ert-deftest mb-lists-explicit-order ()
   (should (l-explicit*< 'a 'c '(a b c)))
   (should (eq (l-explicit*< 'a 'x '(a b c)) 'na))
   (should-not (l-explicit< 'a 'x '(a b c)))
   (should (funcall (explicit*< '(a b c) :test #'eq) 'a 'c))
   (should-not (funcall (explicit< '(a b c) :test #'eq) 'c 'a)))

(ert-deftest mb-lists-pairs-tuples-and-power-set ()
   (should (equal (pairs '(1 2 3 4)) '((1 2) (2 3) (3 4))))
   (should (equal (pairs '(1 2 3) :flank-p t)
                  '((1 1) (1 2) (2 3) (3 3))))
   (should (equal (tuples '(a b c d e) 3)
                  '((a b c) (b c d) (c d e))))
   (should (equal (power-set-indices 2) '(nil (0) (1) (1 0))))
   (should (equal (power-set '(a b)) '(nil (a) (b) (b a)))))

(ert-deftest mb-lists-cut ()
   (should (equal (cut (0-n 5) 3) '((0 1 2))))
   (should (equal (cut (0-n 5) 3 t) '((0 1 2) (3 4))))
   (should (equal (cut '(1 2 3 4) 2) '((1 2) (3 4))))
   (should-error (cut '(1 2) 0))
   (should (equal (cut-list-if #'cl-evenp (0-n 6) t)
                  '((0 1) (2 3) (4 5))))
   (should (equal (cut-list-if #'cl-evenp (0-n 6) nil)
                  '((1) (3) (5)))))

(ert-deftest mb-lists-relations-and-repetitions ()
   (should (equal (relations '(a b c)) '((a b) (a c) (b c))))
   (should (equal (relations '(a b) nil t) '((a b) (b a))))
   (should (equal (repetitions '(a a a))
                  '(((0 1) 2) ((1 1) 1))))
   (should (equal (x-repetitions '(a b a b a b))
                  '(((a b) 3) ((b a) 2)))))

(ert-deftest mb-lists-padding-and-version-order ()
   (should (equal (pad-lists '((1) (1 1) (1 1 0))
                             :elt 'q :from-end nil)
                  '((q q 1) (q 1 1) (1 1 0))))
   (should (equal (pad-lists '((1) (1 1)) :elt 0)
                  '((1 0) (1 1))))
   (should-not (pad-lists nil))
   (should (vlist< '(1 1) '(1 2)))
   (should-not (vlist< '(1 2) '(1 1))))

(ert-deftest mb-lists-rotation-and-unzip ()
   (should (equal (rotate-list '(a b c d e) 2) '(c d e a b)))
   (let ((list (list 'a 'b 'c)))
     (rotate-list list)
     (should (equal list '(a b c))))
   (should (equal (mrotate-list (list 'a 'b 'c 'd) 0 2 3)
                  '(c b d a)))
   (should (equal (unzip (0-n 6)) '((0 2 4) (1 3 5))))
   (should (equal (unzip (0-n 9) 3)
                  '((0 3 6) (1 4 7) (2 5 8)))))

(ert-deftest mb-lists-sorted-list-and-tree ()
   (should (equal (ninsert-sorted-list 1 nil) '(1)))
   (should (equal (ninsert-sorted-list 1 (list 0 2)) '(0 1 2)))
   (should (equal (ninsert-sorted-list 3 (list 0 2)) '(0 2 3)))
   (should (equal (collect-sorted-tree (test-insert-sorted-tree))
                  '(5 7 7 8 11 14))))

(ert-deftest mb-lists-map-and-resize ()
   (should (equal (mapcol #'1+ 1 '((a 1) (b 2)))
                  '((a 2) (b 3))))
   (should (equal (expand-repeat '(1 2 3 4) 6)
                  '(1 2 3 4 1 2 3 4 1 2)))
   (should (equal (resize-list '(1 2 4) 6) '(1 2 4 1 2 4)))
   (should (equal (resize-list '(1 2 4) 2) '(1 2))))

(provide 'test-mb-lists)

;;; test-mb-lists.el ends here
