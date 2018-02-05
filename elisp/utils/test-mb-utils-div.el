(require 'ert)
(require 'mb-utils-div)

(ert-deftest test-all-equal ()
  "Test of `all-equal'"
  (should (equal (all-equal) t))
  (should (equal (all-equal 1) t))
  (should (equal (all-equal 1 1) t))
  (should (equal (all-equal 1 2) nil)))

(ert-deftest test-push-list ()
  "Test of `push-list'"
  (let ((list '(a))
	(prefix '(pre fix)))
    (should (equal (push-list '() list) '(a)))
    (should (equal list '(a)))
    (should (equal (push-list prefix list) '(pre fix a)))
    (should (equal list '(pre fix a)))
    (should (equal prefix '(pre fix)))))

(ert-deftest test-push-back-list ()
"Test of `push-back'"
   (let ((list '(a))
	 (suffix '(suf fix)))
    (should (equal (push-back-list '() list) '(a)))
    (should (equal list '(a)))
    (should (equal (push-back-list suffix list) '(a suf fix)))
    (should (equal list '(a suf fix)))
    (should (equal suffix '(suf fix)))))

(ert-deftest test-last-elt ()
"Test of `last-elt'"
 (should (equal (last-elt '(a b c) 0) 'c))
 (should (equal (last-elt '(a b c) 1) 'b))
 (should (equal (last-elt '(a b c)) 'c)))

(ert-deftest test-push-back ()
  "Test of `push-' macros"
  (let ((list '(a)))
    (should (equal (push-back 'b list) '(a b)))
    (should (equal list '(a b)))))
