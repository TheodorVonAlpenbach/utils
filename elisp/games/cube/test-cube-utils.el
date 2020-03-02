(require 'ert)

(ert-deftest test-cube-revert ()
  "Test of `cube-revert'"
  (should (equal (cube-revert '((R U Uw Rw U2) (U R Uw Rw U2)))
		 '((Uw2 R U Rw Uw) (Uw2 R U Uw Rw)))))

(provide 'test-cube-utils.el)
