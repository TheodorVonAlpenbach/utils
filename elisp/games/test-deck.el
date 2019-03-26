(require 'ert)
(require 'deck)

(ert-deftest test-deck-color ()
  "Test of `deck-color'"
 (should (equal (mapcar #'deck-color (0-n 52))
		(loop for c below 4
		      append (make-list 13 c)))))

(ert-deftest test-deck-value ()
  "Test of `deck-value'"
  (should (equal (mapcar #'deck-value (0-n 52))
		 (loop repeat 4
		       append (0-n 13)))))

(provide 'test-deck)
