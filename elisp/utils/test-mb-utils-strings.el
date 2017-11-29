(require 'ert)
(require 'mb-utils-strings)

(ert-deftest test-string-trim ()
  "Test of `string-trim'."
  (should (equal (string-trim " qwe ") "qwe")))

(ert-deftest test-read-whole-string ()
  "Test of `read-whole-string'."
  (should (equal (read-whole-string "a b (c d)") '(a b (c d)))))

(ert-deftest test-andcat ()
  "Test of `andcat'"
  (should (equal (andcat '()) ""))
  (should (equal (andcat '("Peter")) "Peter"))
  (should (equal (andcat '("Peter" "Mary")) "Peter and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary")) "Peter, Paul, and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary") "; ") "Peter; Paul; and Mary"))
  (should (equal (andcat '("Peter" "Paulus" "Maria") ", " " und " " und ")
	   "Peter, Paulus und Maria")))

