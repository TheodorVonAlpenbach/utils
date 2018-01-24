(require 'ert)
(require 'mb-utils-div)

(ert-deftest test-all-equal ()
  "Test of `all-equal'"
  (should (equal (all-equal) t))
  (should (equal (all-equal 1) t))
  (should (equal (all-equal 1 1) t))
  (should (equal (all-equal 1 2) nil)))

(ert-deftest test-push ()
  "Test of `push-' macros"
  (let ((list '(a)))
    (should (equal (push-back 'b list) '(a b)))
    (should (equal list '(a b)))
    (should (equal (push-back-list '() list) '(a b)))
    (should (equal list '(a b)))
    (should (equal (push-back-list '(c d) list) '(a b c d)))
    (should (equal list '(a b c d)))
    (should (equal (push-list '() list) '(a b c d)))
    (should (equal list '(a b c d)))
    (should (equal (push-list '(pre fix) list) '(pre fix a b c d)))
    (should (equal list '(pre fix a b c d)))))
