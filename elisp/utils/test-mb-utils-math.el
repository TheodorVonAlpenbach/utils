(require 'ert)
(require 'mb-utils-math)

(ert-deftest test-generate-addends-fixed ()
  "Test of `generate-addends-fixed'"
  (should (equal (generate-addends-fixed 577 51)
		 (destructuring-bind (q r) (cl-floor 577 51)
		   (append (make-list r (1+ q))
			   (make-list (- 51 r) q))))))

(ert-deftest test-unsignum ()
  "Test of `unsignum'"
  (should (equal (mapcar #'unsignum '(-2 0 2)) '(1 0 -1)))
  (should (equal (unsignum 1 -1) 1)))

(provide 'test-mb-utils-math)
