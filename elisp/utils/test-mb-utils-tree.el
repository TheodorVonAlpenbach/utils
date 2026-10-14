;;; test-mb-utils-tree.el --- Tests for mb-utils-tree -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-tree)

(ert-deftest test-copy-tree ()
  "Test that the built-in `copy-tree' does not share list structure."
  (should-not (copy-tree nil))
  (should (= (copy-tree 1) 1))
  (let* ((tree (cons (list 1) 2))
         (copy (copy-tree tree)))
    (should (equal tree copy))
    (should-not (eq tree copy))
    (should-not (eq (car tree) (car copy)))
    (setcar (car tree) 2)
    (should-not (equal tree copy))))

(ert-deftest test-relations-tree-round-trip ()
  "Test conversion between relations and trees."
  (let* ((relations '((a b) (a c) (c d) (b e)))
         (tree (car (relations->tree relations))))
    (should (equal tree '(a (c (d)) (b (e)))))
    (should (equal (cl-sort (tree->relations tree) #'string-lessp
                            :key (lambda (relation)
                                   (format "%S" relation)))
                   (cl-sort (copy-tree relations) #'string-lessp
                            :key (lambda (relation)
                                   (format "%S" relation)))))))

(ert-deftest test-tree->relations ()
  "Test of `tree->relations'"
 (should (equal (tree->relations nil) nil)))

(ert-deftest test-find-subtree ()
  "Test of `find-subtree'"
  (should (equal (find-subtree 'f '((a (b (c (d))))
				    (e (f (g (h))))))
		 '(f (g (h)))))
  (should (equal (find-subtree
                  'f '((a (b (f (d)))) (e (f (g (h)))))
                  :filter :up)
                 '((a ((b (f)))))))
  (should (equal (find-subtree
                  'f '((a (b (f (d)))) (e (f (g (h)))))
                  :filter :prune :from-end t)
                 '((e (f (g (h))))))))

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

;;; test-mb-utils-tree.el ends here
