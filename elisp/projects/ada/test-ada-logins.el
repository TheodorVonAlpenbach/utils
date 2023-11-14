(require 'ert)
(require 'ada-logins)

(ert-deftest test-ada-login-infix ()
  "Test of `ada-login-teacher-1'"
  (should
   (equal
    (loop for prefix in (cartesian-product
			 '((9 0 4) (1 3))
			 #'(lambda (x y)
			     (string-to-number
			      (format "%d%d" x y))))
	  collect (ada-login-infix prefix))
    '("499_1" "499_3" "500_1" "500_3" "504_1" "504_3"))))

(provide 'test-ada-logins)
