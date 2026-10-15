;;; test-mb-utils-strings.el --- Tests for mb-utils-strings -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-strings)

(ert-deftest mb-utils-strings-string-trim ()
  "Test of `string-trim'."
  (should (equal (string-trim* " qwe ") "qwe")))

(ert-deftest mb-utils-strings-read-whole-string ()
  "Test of `read-whole-string'."
  (should (equal (read-whole-string "a b (c d)") '(a b (c d)))))

(ert-deftest mb-utils-strings-string-head ()
  "Test of `string-head'"
  (should (equal (string-head "line1\nline2\nline3\nline4") "line1"))
  (should (equal (string-head "line1\nline2\nline3\nline4" 1) "line1"))
  (should (equal (string-head "line1\nline2\nline3\nline4" 2) "line1\nline2"))
  (should (equal (string-head "line1\nline2\nline3\nline4" 0) "")))

(ert-deftest mb-utils-strings-string-tail ()
  "Test of `string-tail'"
  (should (equal (string-tail "line1\nline2\nline3\nline4") "line4"))
  (should (equal (string-tail "line1\nline2\nline3\nline4" 1) "line4"))
  (should (equal (string-tail "line1\nline2\nline3\nline4" 2) "line3\nline4"))
  (should (equal (string-tail "line1\nline2\nline3\nline4" 0) "")))

(ert-deftest mb-utils-strings-string-match ()
"Test of `string-match*'"
 (should (equal (string-match* "\\(e\\)" "sdkjhalkqweee " :num '(0 1 10)) '("e" "e" nil))))

(ert-deftest mb-utils-strings-concat ()
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
		   :test #'cl-oddp
		   :key #'number-to-string
		   :discard-nil t)
		 "13"))
  (should (equal (concat* '(("two" (2)) ("three" (3)) ("four" (4)))
		   :test (compose #'primep
				  (lambda (x)
				    (cl-first (cl-second x))))
		   :key #'cl-first
		   :in " and ")
		 "two and three"))
  (should (equal (concat* '(("a") ("z" "ignored"))
		   :pre "\\([^" :in "-" :suf "]\\)"
		   :key #'cl-first)
		 "\\([^a-z]\\)"))
  (should (equal (concat* '(Peter Paul Mary)
		   :infun #'(lambda (i n) (if (= i (- n 2)) ", and " ", "))
		   :key #'sstring)
		 "Peter, Paul, and Mary")))

(ert-deftest mb-utils-strings-split-string ()
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

(ert-deftest mb-utils-strings-split-string-modify-positions ()
  "Test of `split-string-modify-positions'"
  (should (equal (split-string-modify-positions '((1 2) (5 6)) nil)
		 '((0 1) (2 5) (6))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :none)
		 '((0 1) (2 5) (6))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :left)
		 '((0 1) (1 5) (5))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :right)
		 '((0 2) (2 6) (6))))
  (should (equal (split-string-modify-positions '((1 2) (5 6)) :only)
		 '((1 2) (5 6)))))

(ert-deftest mb-utils-strings-split-string-regexp-list ()
  "Test of `split-string-regexp-list'"
  (should (equal (split-string-regexp-list "babc db efg b " "b" t)
		 '("" "b" "a" "b" "c d" "b" "efg" "b" "")))
  (should (equal (split-string-regexp-list "babc db efg b " "b" nil)
		 '("" "b" "a" "b" "c d" "b" " efg " "b" " "))))

(ert-deftest mb-utils-strings-alliterate-word ()
  "Test of `alliterate-word'"
  (should (equal (alliterate-word "") ""))
  (should (equal (alliterate-word "I") "I"))
  (should (equal (alliterate-word "Me") "Me"))
  (should (equal (alliterate-word "Moi") "Moi"))
  (let* ((word "Mine")
	 (aword (alliterate-word "Mine")))
    (should (equal (first-elt word) (first-elt aword)))
    (should (equal (last-elt word) (last-elt aword)))))

(ert-deftest mb-utils-strings-blanks ()
  "Test of `blanks'"
  (should (equal (blanks 3) "   ")))

(ert-deftest mb-utils-strings-blank-p ()
  "Test of `blank-p'"
  (should (equal (blank-p "") t))
  (should (equal (blank-p " ") t))
  (should (equal (blank-p "\t") t))
  (should (equal (blank-p "\n") t))
  (should (equal (blank-p " \t\n ") t)))

(ert-deftest mb-utils-strings-integer-string-p ()
  "Test of `integer-string-p'"
  (should (equal (mapcar #'integer-string-p
		   '("123" "0" "-123" 123 "a"))
		 '("123" "0" "-123" nil nil)))
  (should (equal (mapcar (bind #'integer-string-p t)
		   '("123" "0" "-123" 123 "a"))
		 '("123" "0" nil nil nil))))

(ert-deftest mb-utils-strings-uuid-regexp ()
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

(ert-deftest mb-utils-strings-uuid-p ()
  "Test of `uuid-p'"
  (should (uuid-p "53bf7368-f985-4061-9283-b3065a578a7f"))
  (should (uuid-p "53bf7368f98540619283b306")))

(ert-deftest mb-utils-strings-commented-string-p ()
  "Test of `commented-string-p'"
  (should (commented-string-p "#comment" "#"))
  (should (commented-string-p "//comment" "//"))
  (should-not (commented-string-p "#comment" "//"))
  (should-not (commented-string-p "" "//" nil))
  (should (commented-string-p "" "//" t)))

(ert-deftest mb-utils-strings-alphabet ()
  "Test of `alphabet'"
  (should (equal (alphabet) "abcdefghijklmnopqrstuvwxyz"))
  (should (equal (alphabet :en) "abcdefghijklmnopqrstuvwxyz"))
  (should (equal (alphabet :no) "abcdefghijklmnopqrstuvwxyzæøå")))

(ert-deftest mb-utils-strings-integer-to-ordinal-string ()
  "Test of `integer-to-ordinal-string'"
  (should
   (equal
    (mapcar #'integer-to-ordinal-string
      '(1 2 3 4 9
	10 11 12 13 14
	20 21 22 23 24
	100003))
    '("first" "second" "third" "fourth" "ninth"
      "tenth" "eleventh" "twelveth" "thirteenth" "fourteenth"
      "twentieth" "twenty-first" "twenty-second" "twenty-third" "twenty-fourth"
      "one hundred thousand third"))))

(ert-deftest mb-utils-strings-integer-to-ordinal ()
  "Test of `integer-to-ordinal'"
  (should
   (equal
    (mapcar #'integer-to-ordinal
      '(1 2 3 4 9 10 11 12 13 14
	20 21 22 23 24 100003))
    '("1st" "2nd" "3rd" "4th" "9th" "10th" "11th" "12th" "13th" "14th"
      "20th" "21st" "22nd" "23rd" "24th" "100003rd") )))

(ert-deftest mb-utils-strings-string-replace-f ()
  "Test of `string-replace-f'"
  (should (equal (string-replace-f
		  "-qwewe-" "\\(qw\\)e"
		  #'(lambda (string) (upcase (match-string 1 string))))
		 "-QWwe-")))

(ert-deftest mb-utils-strings-integer-to-literary-string ()
  "Test of `integer-to-literary-string'"
  (should
   (equal
    (mapcar #'integer-to-literary-string (list 21 99 100 100003))
    '("twenty-one" "ninety-nine" "one hundred" "one hundred thousand three"))))

(ert-deftest mb-utils-strings-string-case ()
  "Test of `string-case'"
  (should (equal (string-case "C" (("a" "b") 'ewq) (("a" "b" "C") 'qwe)) 'qwe))
  (should (equal (string-case "a" (("a" "b") 'ewq) (("a" "b" "C") 'qwe)) 'ewq))
  (should (equal (string-case "a" ("a" 'ewq) ("b" 'qwe)) 'ewq)))

(ert-deftest mb-utils-strings-andcat ()
  "Test of `andcat'"
  (should (equal (andcat '()) ""))
  (should (equal (andcat '("Peter")) "Peter"))
  (should (equal (andcat '("Peter" "Mary")) "Peter and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary")) "Peter, Paul, and Mary"))
  (should (equal (andcat '("Peter" "Paul" "Mary") "; ") "Peter; Paul; and Mary"))
  (should (equal (andcat '("Peter" "Paulus" "Maria") ", " " und " " und ")
	   "Peter, Paulus und Maria")))

(ert-deftest mb-utils-strings-uint8array-string-to-hex-string ()
  "Test of `uint8array-string-to-hex-string'"
  (should (equal (uint8array-string-to-hex-string
		  "{ buffer: new Uint8Array([105, 177, 111, 68, 136, 211]),}")
		 "69b16f4488d3")))

(ert-deftest mb-utils-strings-uint8sequence-to-hex-string ()
  "Test of `uint8sequence-to-hex-string'"
  (should (equal (uint8sequence-to-hex-string
		  [105 177 111 68 136 211 131 162 203 198 86 151])
		 "69b16f4488d383a2cbc65697"))
  (should (equal (uint8sequence-to-hex-string
		  '(105 177 111 68 136 211 131 162 203 198 86 151))
		 "69b16f4488d383a2cbc65697"))
  (should (eql 1 1)))

(ert-deftest mb-utils-strings-basic-conversions ()
  (should (equal (pure-string "text") "text"))
  (should (eql (pure-string [?a ?b]) ?a))
  (should (equal (blanks 3 ?-) "---"))
  (should (equal (copy-case "example" "X") "Example")))

(ert-deftest mb-utils-strings-string-match-end-start ()
  (should (= (string-match-end "a" "a--a" 1) 4))
  (should-not (string-match-end "z" "a--a" 1)))

(ert-deftest mb-utils-strings-substring-interval ()
  (let ((interval (interval-oc "b" "d")))
    (should (equal (substring-intv "abcdefgabcdefg" interval) "cd"))
    (should (equal (substring-intv "abcdefgabcdefg" interval :all)
		   '("cd" "cd")))))

(ert-deftest mb-utils-strings-invalid-input-errors ()
  (should-error (bsg-move "102") :type 'error)
  (should-error (alphabet :unsupported) :type 'error))

;;; test-mb-utils-strings.el ends here
