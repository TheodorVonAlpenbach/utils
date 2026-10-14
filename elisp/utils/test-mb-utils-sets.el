;;; test-mb-utils-sets.el --- Tests for mb-utils-sets -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-sets)

(ert-deftest test-interval-boundaries ()
  "Test open and closed interval boundaries."
  (let ((open (interval-oo 1 3))
        (closed (interval-cc 1 3)))
    (should (open-left-p open))
    (should (open-right-p open))
    (should (closed-left-p closed))
    (should (closed-right-p closed))
    (should (equal (interval-list closed) '(1 3)))
    (should-not (within 1 open))
    (should (within 2 open))
    (should-not (within 3 open))
    (should (within 1 closed))
    (should (within 3 closed))))

(ert-deftest test-1-sphere ()
  "Test one-dimensional closed spheres."
  (let ((sphere (1-sphere 2 10)))
    (should (equal (interval-list sphere) '(8 12)))
    (should (within 8 sphere))
    (should (within 12 sphere))
    (should-not (within 13 sphere))))

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

;;; test-mb-utils-sets.el ends here
