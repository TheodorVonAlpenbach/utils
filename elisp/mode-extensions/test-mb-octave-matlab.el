(require 'ert)
(require 'mb-octave-matlab)

(ert-deftest test-o2m-split-multiassign-string ()
  "Test of `o2m-split-multiassign-string'"
 (should (equal (o2m-split-multiassign-string "  a = b = c = v; ")
		"  c = v;\n  b = c;\n  a = b;"))
 (should (equal (o2m-split-multiassign-string "  a = b = c = v; " 0)
		"c = v;\nb = c;\na = b;")))

(ert-deftest test-o2m-convert-arguments ()
  "Test of `o2m-convert-arguments'"
  (should
   (equal
    (o2m-convert-arguments '(("a") ("b" "v") ("c" "u")))
    '("a, b, c"
      "  if (nargin < 2)\n    b = v;\n  end\n  if (nargin < 3)\n    c = u;\n  end\n\n")))
  (should
   (equal
    (o2m-convert-arguments '(("a") ("b") ("c")))
    '("a, b, c" ""))))

(ert-deftest test-o2m-convert-assert ()
  "Test of `o2m-convert-assert'"
  (should (equal (o2m-convert-assert "assert (a, b);")
		 "  verifyEqual (testCase, a, b);"))
  (should (equal (o2m-convert-assert "assert (a, b, tol);")
		 "  verifyEqual (testCase, a, b, tol);")))

(ert-deftest test-o2m-convert-test-line ()
  "Test of `o2m-convert-test-line'"
  (should (equal (o2m-convert-test-line "assert (a, b);")
		 "  verifyEqual (testCase, a, b);"))
  (should (equal (o2m-convert-test-line "a = b + c;")
		 "  a = b + c;"))
  (should (equal (o2m-convert-test-line "test") nil)))

(ert-deftest test-o2m-convert-test-string ()
  "Test of `o2m-convert-test-string'"
  (should (equal (o2m-convert-test-string
		  "%!test\n%! x = randi (100, 10);\n%! assert (a, b);")
		 "  x = randi (100, 10);\n  verifyEqual (testCase, a, b);")))

(provide 'test-mb-octave-matlab)
