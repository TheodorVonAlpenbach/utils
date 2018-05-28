(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-minimum ()
"Test of `positions'"
 (should (equal (minimum '(1 2 3 5 5) :start 3) '(5 3 5)))
 (should (equal (minimum '(1 2 3 5 5)) '(1 0 1)))
)

(ert-deftest test-positions ()
"Test of `positions'"
 (should (equal (positions 'a '(a b c a)) '(0 3)))
 (should (equal (positions '(a) '(a b c a)) '(0 3)))
 (should (equal (positions '(a b b) '(a b c a)) '(0 1 3)))
 (should (equal (positions "a" '("a" "b" "c" "a")) nil))
 (should (equal (positions "a" '("a" "b" "c" "a") :test #'string=) '(0 3)))
 (should (equal (positions '(a) '((a) (b) (c) (a)) :key #'car) '(0 3))))

(ert-deftest test-minimum ()
"Test of `minimum'"
 (should (equal (minimum '(1 2 3 5 5) :start 3) '(5 3 5))))

(ert-deftest test-insert-sequence ()
  "Test of `insert-sequence'"
  (should (all-equal (insert-sequence "15010" "_" :start1 2)
		     (insert-sequence "15010" "_" :start1 2 :end1 2)
		     (insert-sequence "15010" "_" :start1 2 :end1 -3)
		     (insert-sequence "15010" "_" :start1 -3 :end1 -3)
		     (insert-sequence "15010" "_" :start1 -3 :end1 2)
		     "15_010"))
  (should (equal (insert-sequence "15010" "abc")
		 "abc15010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1)
		 "1abc5010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1 :end1 2)
		 "1abc010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1 :end1 2 :start2 1)
		 "1bc010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1 :end1 2 :start2 1 :end2 -1)
		 "1b010")))

(ert-deftest test-replace-sequence ()
  "Test of `replace-sequence'"
  (should (equal (replace-sequence "01234" "ab" 1)
		 "0ab34"))
  (should (equal (replace-sequence "01234" "ab" 1 1)
		 "0ab1234"))
  (should (equal (replace-sequence "01234" "ab" 1 -1)
		 "0ab4")))

(provide 'test-mb-sequences.el)
