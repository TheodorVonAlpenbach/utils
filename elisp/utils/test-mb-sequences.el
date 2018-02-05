(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-positions ()
"Test of `positions'"
 (should (equal (positions 'a '(a b c a)) '(0 3)))
 (should (equal (positions '(a) '(a b c a)) '(0 3)))
 (should (equal (positions '(a b b) '(a b c a)) '(0 1 3)))
 (should (equal (positions "a" '("a" "b" "c" "a")) nil))
 (should (equal (positions "a" '("a" "b" "c" "a") :test #'string=) '(0 3)))
 (should (equal (positions '(a) '((a) (b) (c) (a)) :key #'car) '(0 3))))

(provide 'test-mb-sequences.el)
