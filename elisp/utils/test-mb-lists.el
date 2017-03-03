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

