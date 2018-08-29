(require 'ert)

(ert-deftest test-set-equalp ()
  "Test of `set-equalp'"
  (should (equal (set-equalp '(a) '(a a)) t))
  (should-not (set-equalp '(a) '(a b)))
  (should-not (set-equalp '(a b) '(a)))
  (should-not (set-equalp '((a)) '((a) (a b))))
  (should (equal (set-equalp '((a)) '((a) (a b)) :key #'car) t))
  (should-not (equal (set-equalp '("a") '("a" "a")) t))
  (should (equal (set-equalp '("a") '("a" "a") :test #'string=) t)))

(provide 'test-mb-utils-sets.el)
