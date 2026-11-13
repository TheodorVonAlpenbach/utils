;;; test-mb-utils-sets.el --- Tests for mb-utils-sets -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-sets)

(ert-deftest mb-utils-sets-interval-boundaries ()
  "Test open and closed interval boundaries."
  (let ((open (interval-oo 1 3))
        (open-closed (interval-oc 1 3))
        (closed-open (interval-co 1 3))
        (closed (interval-cc 1 3)))
    (should (open-left-p open))
    (should (open-right-p open))
    (should (closed-left-p closed))
    (should (closed-right-p closed))
    (should (equal (interval-list closed) '(1 3)))
    (should-not (within 1 open))
    (should (within 2 open))
    (should-not (within 3 open))
    (should-not (within 1 open-closed))
    (should (within 3 open-closed))
    (should (within 1 closed-open))
    (should-not (within 3 closed-open))
    (should (within 1 closed))
    (should (within 3 closed))))

(ert-deftest mb-utils-sets-interval-ordering ()
  (should (within "b" (interval-cc "a" "c") :test #'string<))
  (should-not (within "a" (interval-oo "a" "c") :test #'string<))
  (should-not (within 2 (interval-cc 3 1)))
  (should (within 1 (interval-cc 1 1)))
  (should-not (within 1 (interval-oc 1 1)))
  (should-not (within 1 (interval-co 1 1)))
  (should-not (within 1 (interval-oo 1 1))))

(ert-deftest mb-utils-sets-1-sphere ()
  "Test one-dimensional closed spheres."
  (let ((sphere (1-sphere 2 10)))
    (should (equal (interval-list sphere) '(8 12)))
    (should (within 8 sphere))
    (should (within 12 sphere))
    (should-not (within 13 sphere)))
  (should (equal (interval-list (1-sphere 0 10)) '(10 10)))
  (should (equal (interval-list (1-sphere 2)) '(-2 2)))
  (should-error (1-sphere -1) :type 'user-error)
  (should-error (1-sphere "1") :type 'wrong-type-argument)
  (should-error (1-sphere 1 "0") :type 'wrong-type-argument))

(ert-deftest mb-utils-sets-set-equalp ()
  "Test of `set-equalp'"
  (should (equal (set-equalp '(a) '(a a)) t))
  (should-not (set-equalp '(a) '(a b)))
  (should-not (set-equalp '(a b) '(a)))
  (should-not (set-equalp '((a)) '((a) (a b))))
  (should (equal (set-equalp '((a)) '((a) (a b)) :key #'car) t))
  (should-not (equal (set-equalp '("a") '("a" "a")) t))
  (should (equal (set-equalp '("a") '("a" "a") :test #'string=) t)))

(provide 'test-mb-utils-sets)

;;; test-mb-utils-sets.el ends here
