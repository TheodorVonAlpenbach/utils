;;; test-mb-octave-matlab.el --- Tests for Octave conversion -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-octave-matlab)

(ert-deftest mb-o2m-split-multiassign-string ()
  "Test of `o2m-split-multiassign-string'"
 (should (equal (o2m-split-multiassign-string "  a = b = c = v; ")
		"  c = v;\n  b = c;\n  a = b;"))
 (should (equal (o2m-split-multiassign-string "  a = b = c = v; " 0)
		"c = v;\nb = c;\na = b;")))

(ert-deftest mb-o2m-convert-arguments ()
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

(ert-deftest mb-o2m-convert-assert ()
  "Test of `o2m-convert-assert'"
  (should (equal (o2m-convert-assert "assert (a, b);")
		 "  verifyEqual (testCase, a, b);"))
  (should (equal (o2m-convert-assert "assert (a, b, tol);")
		 "  verifyEqual (testCase, a, b, tol);")))

(ert-deftest mb-o2m-convert-test-line ()
  "Test of `o2m-convert-test-line'"
  (should (equal (o2m-convert-test-line "a = b + c;")
		 "  a = b + c;"))
  (should (equal (o2m-convert-test-line "test") nil)))

(ert-deftest mb-o2m-register-version-swaps ()
  (let ((*version-swaps* nil)
        (o2m-octave-root "/tmp/octave")
        (o2m-matlab-root "/tmp/matlab"))
    (should (= (length (o2m-register-version-swaps)) 8))
    (should
     (member '("/tmp/octave/math" "/tmp/matlab/math")
             *version-swaps*))
    (o2m-register-version-swaps)
    (should (= (length *version-swaps*) 8))))

(ert-deftest mb-o2m-convert-colon-string ()
  (should
   (equal
    (o2m-convert-colon-string "  foo = barcode (x)(:);" "  ")
    (concat "  tmp_foo_ = barcode (x);\n"
            "  foo = tmp_foo_(:);")))
  (should-error (o2m-convert-colon-string "foo = bar (x);" "")))

(ert-deftest mb-o2m-convert-test-string ()
  (should
   (equal
    (o2m-convert-test-string
     (concat "%!test\n"
             "%! x = randi (100, 10);\n"
             "%! assert (x, expected);"))
    (concat "  x = randi (100, 10);\n"
            "  verifyEqual (testCase, x, expected);"))))

(ert-deftest mb-o2m-buffer-conversions ()
  (let* ((directory (make-temp-file "mb-o2m-" t))
         (o2m-matlab-root directory))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "example.m" directory))
          (insert (concat "if (!isna (x))\n"
                          "## comment {}\n"
                          "  a = b = c;\n"
                          "endif\n"))
          (o2m-convert-ends)
          (o2m-comments-buffer)
          (o2m-convert-NA)
          (o2m-convert-empty-curls)
          (o2m-not-ify-buffer)
          (o2m-split-multiassign)
          (should
           (equal
            (buffer-string)
            (concat "if (~isnan (x))\n"
                    "% comment {}\n"
                    "  b = c;\n"
                    "  a = b;\n"
                    "end\n"))))
      (delete-directory directory))))

(provide 'test-mb-octave-matlab)

;;; test-mb-octave-matlab.el ends here
