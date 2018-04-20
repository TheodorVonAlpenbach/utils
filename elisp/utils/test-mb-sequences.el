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

(provide 'test-mb-sequences.el)
