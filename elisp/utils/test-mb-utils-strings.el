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

(ert-deftest test-split-string* ()
  "Test of `split-string*'"
 (should (equal (split-string* " 09:16  Text" "[0-2][0-9]:[0-5][0-9]")
		'(" " "  Text")))
 (should (equal (split-string* " 09:16  Text" "[0-2][0-9]:[0-5][0-9]" :none)
		'(" " "  Text")))
 (should (equal (split-string* " 09:16  Text" "[0-2][0-9]:[0-5][0-9]" :left)
		'(" " "09:16  Text")))
 (should (equal (split-string* " 09:16  Text" "[0-2][0-9]:[0-5][0-9]" :right)
		'(" 09:16" "  Text")))
 (should (equal (split-string* " 09:16  Text" "[0-2][0-9]:[0-5][0-9]" :only)
		'("09:16"))))

(ert-deftest test-split-string-modify-positions ()
  "Test of `split-string-modify-positions'"
  (should (equal (split-string-modify-positions '((1 2) (5 6)) nil) '((0 1) (2 5) (6))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :none) '((0 1) (2 5) (6))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :left) '((0 1) (1 5) (5))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :right) '((0 2) (2 6) (6))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :only) '((1 2) (5 6)))))

(ert-deftest test-split-string-regexp-list ()
  "Test of `split-string-regexp-list'"
  (should (equal (split-string-regexp-list "babc db efg b " "b" t)
		 '("" "b" "a" "b" "c d" "b" "efg" "b" "")))
  (should (equal (split-string-regexp-list "babc db efg b " "b" nil)
		 '("" "b" "a" "b" "c d" "b" " efg " "b" " "))))

(ert-deftest test-alliterate-word ()
  "Test of `alliterate-word'"
  (should (equal (alliterate-word "") ""))
  (should (equal (alliterate-word "I") "I"))
  (should (equal (alliterate-word "Me") "Me"))
  (should (equal (alliterate-word "Moi") "Moi"))
  (let* ((word "Mine")
	 (aword (alliterate-word "Mine")))
    (should (equal (first-elt word) (first-elt aword)))
    (should (equal (last-elt word) (last-elt aword)))))

(ert-deftest test-blanks ()
  "Test of `blanks'"
  (should (equal (blanks 3) "   ")))

(ert-deftest test-blank-p ()
  "Test of `blank-p'"
  (should (equal (blank-p "") t))
  (should (equal (blank-p " ") t))
  (should (equal (blank-p "\t") t))
  (should (equal (blank-p "\n") t))
  (should (equal (blank-p " \t\n ") t)))

(ert-deftest test-integer-string-p ()
  "Test of `integer-string-p'"
  (should (equal (mapcar #'integer-string-p
		   '("123" "0" "-123" 123 "a"))
		 '("123" "0" "-123" nil nil)))
  (should (equal (mapcar (bind #'integer-string-p t)
		   '("123" "0" "-123" 123 "a"))
		 '("123" "0" nil nil nil))))

(ert-deftest test-uuid-regexp ()
  "Test of `uuid-regexp'"
  (should (equal (uuid-regexp)
		 "[a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}"))
  (should (equal (uuid-regexp nil) (uuid-regexp)))
  (should (equal (uuid-regexp nil nil) (uuid-regexp)))
  (should (equal (uuid-regexp nil "_")
		 "[a-f0-9]\\{8\\}_[a-f0-9]\\{4\\}_[a-f0-9]\\{4\\}_[a-f0-9]\\{4\\}_[a-f0-9]\\{12\\}"))
  (should (equal (uuid-regexp 3 "_") "[a-f0-9]\\{3\\}"))
  (should (equal (uuid-regexp '(3)) (uuid-regexp 3)))
  (should (equal (uuid-regexp '(2 1)) "[a-f0-9]\\{2\\}-[a-f0-9]\\{1\\}"))
  (should (equal (uuid-regexp '(2 1) nil) (uuid-regexp '(2 1))))
  (should (equal (uuid-regexp '(2 1) "qwe") "[a-f0-9]\\{2\\}qwe[a-f0-9]\\{1\\}")))

(ert-deftest test-uuid-p ()
  "Test of `uuid-p'"
  (should (uuid-p "53bf7368-f985-4061-9283-b3065a578a7f"))
  (should (uuid-p "53bf7368f98540619283b306")))

(ert-deftest test-andcat ()
  "Test of `andcat'"
  (should (equal (andcat '()) ""))
  (should (equal (andcat '("Peter")) "Peter"))
  (should (equal (andcat '("Peter" "Mary")) "Peter and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary")) "Peter, Paul, and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary") "; ") "Peter; Paul; and Mary"))
  (should (equal (andcat '("Peter" "Paulus" "Maria") ", " " und " " und ")
	   "Peter, Paulus und Maria")))
