(require 'ert)
(require 'mb-lists)

(ert-deftest test-combine-1 ()
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

(ert-deftest test-combine ()
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

(ert-deftest test-infix-list ()
  "Test of `infix-list'"
  (should (equal (infix-list '(a b c) #'1+ t)
		 '(a 1 b 2 c)))
  (should (equal (infix-list '(a b c)
			     #'(lambda (i n) (if (= i (- n 2)) 'and '\,)) t)
		 '(a \, b and c))))

(ert-deftest test-repeat-elements ()
  "Test of `repeat-elements'"
  (should (equal (repeat-elements (0-n 3) 3) '(0 0 0 1 1 1 2 2 2)))
  (should (equal (repeat-elements (0-n 3)) '(0 0 1 1 2 2)))
  (should (equal (repeat-elements (0-n 3) 2) '(0 0 1 1 2 2)))
  (should (equal (repeat-elements (0-n 3) 1) '(0 1 2)))
  (should (equal (repeat-elements (0-n 3) 0) nil)))

(ert-deftest test-zip ()
  "Test of `zip'"
  (should (equal (zip nil) nil))
  (should (equal (zip '(0 2 4) '(1 3 3)) '(0 1 2 3 4 3)))
  (should (equal (butlast (zip '(1 2 3) 'og)) '(1 og 2 og 3))))

(ert-deftest test-transpose ()
  "Test of `transpose'"
  (should (equal (transpose nil) nil))
  (should (equal (transpose '((a b c) (d e f))) '((a d) (b e) (c f)))))

(ert-deftest test-rotatef-list ()
  "Test of `rotatef-list'"
  (let ((l '(a b c)))
    (should (equal (rotatef-list l) '(b c a)))
    (should (equal l '(b c a)))
    (rotatef-list l 1)
    (should (equal l '(c a b)))
    (rotatef-list l 2)
    (should (equal l '(b c a)))))

(ert-deftest test-swap ()
  "Test of `gen-swap'"
  (should (equal (swap 'a '((a b) (c d))) 'b))
  (should (equal (swap 'b '((a b) (c d))) 'a))
  (should (equal (swap 'c '((a b) (c d))) 'd))
  (should (equal (swap 'd '((a b) (c d))) 'c))
  (should-not (equal (swap "a" '(("a" "b") ("c" "d"))) "b"))
  (should (equal (swap "a" '(("a" "b") ("c" "d")) :test #'string=) "b")))

(ert-deftest test-group ()
  "Test of `group'"
  (should (equal (group '(a b d a d a b)
		   :test #'(lambda (x y) (eql y 'd)))
		 '((a) (b d) (a d) (a) (b)))))

(ert-deftest test-equivalence-class ()
  "Test of `equivalence-class"
  (should (equal (equivalence-class '(a b d a d a b)) '((a a a) (b b) (d d))))
  (should (equal (equivalence-class '((a) (b) (c) (a) (b) (a) (d)) :key #'car)
		 '(((a) (a) (a)) ((b) (b)) ((c)) ((d))))))

(ert-deftest test-head ()
  "Test of `head'"
 (should (equal (head 0 '(a b c)) nil))
 (should (equal (head 1 '(a b c)) '(a))))

(ert-deftest test-accumulate-list ()
  "Test of `accumulate-list'"
  (should (equal (accumulate-list '(a b c a b a d) :test #'symbol<)
		 '((a 3) (b 2) (c 1) (d 1))))
  (should (equal (accumulate-list '("a" "b" "c" "a" "b" "a" "d") :test #'string<)
		 '(("a" 3) ("b" 2) ("c" 1) ("d" 1)))))

(ert-deftest test-n-0 ()
  "Test of `n-0'"
  (should (equal (n-0 3) '(2 1 0))))

(ert-deftest test-equivalence-class-with-key ()
  "Test of `equivalence-class-with-key'"
 (should (equal (equivalence-class-with-key '("a" "ab" "abc" "b" "bc" "bcd")
					    :key #'length)
		'((1 ("b" "a")) (2 ("bc" "ab")) (3 ("bcd" "abc")))))
 (should (equal (equivalence-class-with-key '("a" "ab" "abc" "b" "bc" "bcd")
					    :key #'(lambda (x) (substring x 0 1))
					    :test #'equal)
		'(("a" ("abc" "ab" "a")) ("b" ("bcd" "bc" "b"))))))

(ert-deftest test-listify-atoms ()
  "Test of `listify-atoms'"
  (should (equal (listify-atoms '(a (1 2 3) (d f))) '((a a a) (1 2 3) (d f)))))

(ert-deftest test-memcase ()
  "Test of `memcase'"
  (should (equal (memcase '(a b c) (a 'A) (otherwise 'B)) 'A))
  (should (equal (memcase '(a b c) (d 'A) (otherwise 'B)) 'B))
  (should (equal (memcase '(a b c) ((a d) 'A) (otherwise 'B)) 'A))
  (should (equal (memcase '(a b c) ((d e) 'A) (otherwise 'B)) 'B)))

(ert-deftest test-headtail ()
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

(ert-deftest test-cars ()
  "Test of `cars'"
  (should (equal (cars '((a b c) (d e))) '(a d))))
