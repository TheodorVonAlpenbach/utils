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

(provide 'test-mb-octave-matlab)
