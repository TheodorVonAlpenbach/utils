(require 'ert)
(require 'smart-swap)

(ert-deftest test-gen-swap ()
  "Test of `gen-swap'"
  (should (equal (gen-swap 'a '((a b) (c d))) 'b))
  (should (equal (gen-swap 'b '((a b) (c d))) 'a))
  (should (equal (gen-swap 'c '((a b) (c d))) 'd))
  (should (equal (gen-swap 'd '((a b) (c d))) 'c))
  (should-not (equal (gen-swap "a" '(("a" "b") ("c" "d"))) "b"))
  (should (equal (gen-swap "a" '(("a" "b") ("c" "d")) :test #'string=) "b")))

(provide 'test-smart-swap)
