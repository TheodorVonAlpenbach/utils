(require 'ert)
(require 'oll)

(ert-deftest test-oll-expand-corners ()
  "Test of `oll-expand-corners'"
  (should (equal (oll-expand-corners '(3 4 5 6 9) '(r l) 'y2) '(u l r u))))

(ert-deftest test-oll-uf-p ()
  "Test of `oll-uf-p'"
  (should (oll-uf-p 2 '(1 2 3)))
  (should-not (oll-uf-p 4 '(1 2 3))))

(ert-deftest test-oll-ufs ()
  "Test of `oll-ufs'"
  (should (equal (oll-ufs '(2 3 5 6 7) nil) '(nil t t nil t t t nil nil)))
  (should (equal (oll-ufs '(2 3 5 6 7) 'yw) '(t nil nil nil t t nil t t))))

(ert-deftest test-oll-rotation-number ()
  "Test of `oll-rotation-number'"
 (should (equal (mapcar #'oll-rotation-number '(nil y y2 yw)) '(0 1 2 3))))

(provide 'test-oll.el)
