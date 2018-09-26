(require 'ert)
(require 'mb-lists)

(ert-deftest test-combine ()
  "Test of `combine'"
  (should (equal (combine '()) nil))
  (should (equal (combine '(a)) '((a))))
  (should (equal (combine '(a b)) '((a b))))
  (should (equal (combine '(a (b c))) '((a b) (a c))))
  (should (equal (combine '((a b) c)) '((a c) (b c))))
  (should (equal (combine '((a))) '((a))))
  (should (equal (combine '((a b))) '((a) (b))))
  (should (equal (combine '((a d) (b c))) '((a b) (a c) (d b) (d c))))
  (should (equal (combine '((a) (b) (c)) :key #'list) '((a b c))))
  (should (equal (combine '(("a" "b") ("c") ("e" "f")))
		 '(("a" "c" "e") ("a" "c" "f") ("b" "c" "e") ("b" "c" "f"))))
  (should (equal (combine '(("a" "b") ("c") ("e" "f")) :key #'concat)
		 '("ace" "acf" "bce" "bcf"))))

(ert-deftest test-tree-leaves ()
  "Test of `tree-leaves'"
  (should (equal (tree-leaves '()) nil))
  (should (equal (tree-leaves '(a)) '(a)))
  (should (equal (tree-leaves '(a (b))) '(b)))
  (should (equal (tree-leaves '("a" ("b"))) '("b")))
  (should (equal (tree-leaves '(a (b) (c))) '(b c)))
  (should (equal (tree-leaves '(a (c (d (f))) (b (e)))) '(f e))))

(ert-deftest test-tree-member ()
  "Test of `tree-member'"
  (should (equal (tree-member 'a ()) nil))
  (should (equal (tree-member 'a '(a)) '(a)))
  (should (equal (tree-member 'a '(a (b))) '(a (b))))
  (should (equal (tree-member 2 '("a" ("bb" ("ccc"))) :key #'length)
		 '("bb" ("ccc"))))
  (should (equal (tree-member "b" '("a" ("b" ("c"))) :test #'string=)
		 '("b" ("c"))))
  (should (equal (tree-member 'b '(a (b (c)) (b (d))) :from-end nil) '(b (c))))
  (should (equal (tree-member 'b '(a (b (c)) (b (d))) :from-end t) '(b (d)))))

(ert-deftest test-infix-list ()
  "Test of `infix-list'"
  (should (equal (infix-list '(a b c) #'1+ t)
		 '(a 1 b 2 c)))
  (should (equal (infix-list '(a b c)
			     #'(lambda (i n) (if (= i (- n 2)) 'and '\,)) t)
		 '(a \, b and c))))

(ert-deftest test-memcase ()
  "Test of `memcase'"
  (should (equal (memcase '(a b c) (a 'A) (otherwise 'B)) 'A))
  (should (equal (memcase '(a b c) (d 'A) (otherwise 'B)) 'B))
  (should (equal (memcase '(a b c) ((a d) 'A) (otherwise 'B)) 'A))
  (should (equal (memcase '(a b c) ((d e) 'A) (otherwise 'B)) 'B)))

