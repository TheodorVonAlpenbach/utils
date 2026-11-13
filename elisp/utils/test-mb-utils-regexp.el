;;; test-mb-utils-regexp.el --- Tests for mb-utils-regexp -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-regexp)

(ert-deftest mb-utils-regexp-constants ()
  (should (string-match-p (concat "\\`" *natural-number-regexp* "\\'") "1"))
  (should (string-match-p (concat "\\`" *natural-number-regexp* "\\'") "123"))
  (should-not (string-match-p *natural-number-regexp* "0")))

(ert-deftest mb-utils-regexp-or ()
  (let ((regexp (concat "\\`" (regexp-or "a+" "b?") "\\'")))
    (should (string-match-p regexp "aaa"))
    (should (string-match-p regexp ""))
    (should (string-match-p regexp "b"))
    (should-not (string-match-p regexp "ab")))
  (should-not (string-match-p (regexp-or) "")))

(ert-deftest mb-utils-regexp-or-integers ()
  (let ((regexp (concat "\\`" (regexp-or-integers -1 1 12 300) "\\'")))
    (dolist (string '("-1" "1" "12" "300"))
      (should (string-match-p regexp string)))
    (dolist (string '("" "2" "120"))
      (should-not (string-match-p regexp string))))
  (should (string-match (regexp-or-integers 1 12) "12"))
  (should (equal (match-string 0 "12") "12"))
  (should-not (string-match-p (regexp-or-integers) "")))

(ert-deftest mb-utils-regexp-group-adjacent ()
  (should (equal (group-adjacent '(0 0 1 2 2 2 3 3))
		 '((0 0) (1) (2 2 2) (3 3))))
  (should (equal (group-adjacent '("aa" "ab" "ba")
				 :test #'=
				 :key (lambda (string) (aref string 0)))
		 '(("aa" "ab") ("ba"))))
  (should-not (group-adjacent nil)))

(ert-deftest mb-utils-regexp-factor-list ()
  (let ((input '("ba" "aa" "ab" "bb")))
    (should (equal (factor-list input #'<
				:key (lambda (string) (aref string 0)))
		   '(("aa" "ab") ("ba" "bb"))))
    (should (equal input '("ba" "aa" "ab" "bb")))))

(ert-deftest mb-utils-regexp-factor-strings ()
  (let ((input '("ab" "b" "ac" "")))
    (should (equal (factor-strings input)
		   '((?a ("b" "c")) (?b nil))))
    (should (equal input '("ab" "b" "ac" ""))))
  (should-not (factor-strings '("" "")))
  (should (equal (remove-empty-strings '("" "a" "" "b"))
		 '("a" "b"))))

(ert-deftest mb-utils-regexp-not-strings ()
  (dolist (blocked
	   (list nil
		 '("")
		 '("ab" "ac")
		 '("a" "ab")
		 '("-" "]" "^" "\\")
		 '("a\nb")
		 '("duplicate" "duplicate")))
    (let ((regexp (mb-regexp-not-strings blocked)))
      (dolist (string blocked)
	(should-not (string-match-p regexp string)))
      (dolist (string '("" "a" "ab" "ac" "ad" "x" "xb"
			 "-" "]" "^" "\\" "a\nb" "a\nc" "duplicate"
			 "duplicate-more"))
	(unless (member string blocked)
	  (should (string-match-p regexp string)))))))

(ert-deftest mb-utils-regexp-input-validation ()
  (should-error (regexp-or-integers 1 2.0)
                :type 'wrong-type-argument)
  (should-error (mb-regexp-not-strings "blocked")
                :type 'wrong-type-argument)
  (should-error (mb-regexp-not-strings '("valid" 1))
                :type 'wrong-type-argument)
  (let ((regexp (mb-regexp-not-strings '("blocked\nvalue"))))
    (should-not (string-match-p regexp "blocked\nvalue"))
    (should (string-match-p regexp "blocked\nother"))))

(provide 'test-mb-utils-regexp)

;;; test-mb-utils-regexp.el ends here
