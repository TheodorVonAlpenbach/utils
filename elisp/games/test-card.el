(require 'ert)
(require 'card)

(ert-deftest test-card-color ()
  "Test of `card-color'"
 (should (equal (mapcar #'card-color (0-n 52))
		(loop for c below 4
		      append (make-list 13 c)))))

(ert-deftest test-card-value ()
  "Test of `card-value'"
  (should (equal (mapcar #'card-value (0-n 52))
		 (loop repeat 4
		       append (0-n 13)))))

(ert-deftest test-card-red-p ()
  "Test of `card-red-p'"
  (should (equal (mapcar #'card-red-p '(0 13 26 39))
		 '(nil t t nil))))

(ert-deftest test-card-black-p ()
  "Test of `card-black-p'"
  (should (equal (mapcar #'card-black-p '(0 13 26 39))
		 '(t nil nil t))))

(ert-deftest test-card-ace-p ()
  "Test of `card-ace-p'"
  (should (equal (copy-if #'card-ace-p (0-n 52)) '(0 13 26 39))))

(ert-deftest test-card-king-p ()
  "Test of `card-king-p'"
 (should (equal (copy-if #'card-king-p (0-n 52)) '(12 25 38 51))))

(provide 'test-card)
