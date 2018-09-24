(require 'ert)
(require 'gui)

(ert-deftest test-gui-align-horizontally ()
  "Test of `gui-align-horizontally'"
  (should (equal (gui-align-horizontally
		  nil '(:rectangle 1) '(:stretch 1) '(:space 1))
		 '((:rectangle 1) (:space 1)))))

(provide 'test-gui)
