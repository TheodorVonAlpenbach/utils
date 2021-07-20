(require 'ert)
(require 'mb-utils-tree)

(ert-deftest test-tree->relations ()
  "Test of `tree->relations'"
 (should (equal (tree->relations nil) nil)))

(ert-deftest test-find-subtree ()
  "Test of `find-subtree'"
  (should (equal (find-subtree 'f '((a (b (c (d))))
				    (e (f (g (h))))))
		 '(f (g (h))))))

(ert-deftest test-tree-leaves ()
  "Test of `tree-leaves'"
  (should (equal (tree-leaves '()) nil))
  (should (equal (tree-leaves '(a)) '(a)))
  (should (equal (tree-leaves '(a (b))) '(b)))
  (should (equal (tree-leaves '("a" ("b"))) '("b")))
  (should (equal (tree-leaves '(a (b) (c))) '(b c)))
  (should (equal (tree-leaves '(a (c (d (f))) (b (e)))) '(f e))))

(ert-deftest test-tree-member ()
  "Test of `tree-member'"
  (should (equal (tree-member 'a ()) nil))
  (should (equal (tree-member 'a '(a)) '(a)))
  (should (equal (tree-member 'a '(a (b))) '(a (b))))
  (should (equal (tree-member 2 '("a" ("bb" ("ccc"))) :key #'length)
		 '("bb" ("ccc"))))
  (should (equal (tree-member "b" '("a" ("b" ("c"))) :test #'string=)
		 '("b" ("c"))))
  (should (equal (tree-member 'b '(a (b (c)) (b (d))) :from-end nil) '(b (c))))
  (should (equal (tree-member 'b '(a (b (c)) (b (d))) :from-end t) '(b (d)))))

(provide 'test-mb-utils-tree)
