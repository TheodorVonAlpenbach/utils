(require 'ert)
(require 'mb-utils-tree)

(ert-deftest test-tree->relations ()
  "Test of `tree->relations'"
 (should (equal (tree->relations ) nil)))

(ert-deftest test-find-subtree ()
  "Test of `find-subtree'"
  (should (equal (find-subtree 'f '((a (b (c (d))))
				    (e (f (g (h))))))
		 '(f (g (h))))))

(provide 'test-mb-utils-tree)
