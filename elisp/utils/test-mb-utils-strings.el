(require 'ert)
(require 'mb-utils-strings)

(ert-deftest test-string-trim* ()
  "Test of `string-trim'."
  (should (equal (string-trim* " qwe ") "qwe")))

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

(ert-deftest test-concat* ()
  "Test of `concat*'"
  (should (equal (concat* '("1" "" "3")
		   :in "\n"
		   :indent-string ">>"
		   :discard-empty t)
		 ">>1\n>>3"))
  (should (equal (concat* '("1" "2" "3") :in "\n" :indent-string ">>")
		 ">>1\n>>2\n>>3"))
  (should (equal (concat* '("a" "b" "c")
		   :pre "(" :in " " :suf ")"
		   :test #'(lambda (x) (string= x "a"))
		   :key (compose #'number-to-string #'length))
		 "(1)"))
  (should (equal (concat* '(1 2 nil 3)
		   :test #'oddp
		   :key #'number-to-string
		   :discard-nil t)
		 "13"))
  (should (equal (concat* '(("two" (2)) ("three" (3)) ("four" (4)))
		   :test (compose #'primep #'caadr)
		   :key #'first
		   :in " and ")
		 "two and three"))
  (should (equal (concat* '(("a") ("z" "ignored"))
		   :pre "\\([^" :in "-" :suf "]\\)"
		   :key #'car)
		 "\\([^a-z]\\)"))
  (should (equal (concat* '(Peter Paul Mary)
		   :infun #'(lambda (i n) (if (= i (- n 2)) ", and " ", "))
		   :key #'sstring)
		 "Peter, Paul, and Mary")))

(ert-deftest test-andcat ()
  "Test of `andcat'"
  (should (equal (andcat '()) ""))
  (should (equal (andcat '("Peter")) "Peter"))
  (should (equal (andcat '("Peter" "Mary")) "Peter and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary")) "Peter, Paul, and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary") "; ") "Peter; Paul; and Mary"))
  (should (equal (andcat '("Peter" "Paulus" "Maria") ", " " und " " und ")
	   "Peter, Paulus und Maria")))

