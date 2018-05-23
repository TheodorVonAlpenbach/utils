(require 'ert)
(require 'mb-utils-strings)

(ert-deftest test-string-trim ()
  "Test of `string-trim'."
  (should (equal (string-trim " qwe ") "qwe")))

(ert-deftest test-read-whole-string ()
  "Test of `read-whole-string'."
  (should (equal (read-whole-string "a b (c d)") '(a b (c d)))))

(ert-deftest test-string-head ()
  "Test of `string-head'"
  (should (equal (string-head "line1\nline2\nline3\nline4") "line1"))
  (should (equal (string-head "line1\nline2\nline3\nline4" 1) "line1"))
  (should (equal (string-head "line1\nline2\nline3\nline4" 2) "line1\nline2"))
  (should (equal (string-head "line1\nline2\nline3\nline4" 0) "")))

(ert-deftest test-string-tail ()
  "Test of `string-tail'"
  (should (equal (string-tail "line1\nline2\nline3\nline4") "line4"))
  (should (equal (string-tail "line1\nline2\nline3\nline4" 1) "line4"))
  (should (equal (string-tail "line1\nline2\nline3\nline4" 2) "line3\nline4"))
  (should (equal (string-tail "line1\nline2\nline3\nline4" 0) "")))



(ert-deftest test-string-match* ()
"Test of `string-match*'"
 (should (equal (string-match* "\\(e\\)" "sdkjhalkqweee " :num '(0 1 10)) '("e" "e" nil))))

(ert-deftest test-andcat ()
  "Test of `andcat'"
  (should (equal (andcat '()) ""))
  (should (equal (andcat '("Peter")) "Peter"))
  (should (equal (andcat '("Peter" "Mary")) "Peter and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary")) "Peter, Paul, and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary") "; ") "Peter; Paul; and Mary"))
  (should (equal (andcat '("Peter" "Paulus" "Maria") ", " " und " " und ")
	   "Peter, Paulus und Maria")))

