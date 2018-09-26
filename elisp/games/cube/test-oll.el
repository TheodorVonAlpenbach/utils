(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-oll-expand-corners ()
  "Test of `oll-expand-corners'"
  (should (equal (oll-expand-corners '(2 3 5 6 7) '(1 0)) '(1 nil 0 nil))))

(ert-deftest test-oll-uf-xy ()
  "Test of `oll-uc-xy'"
  (should (equal (oll-uf-xy '(2 3 5 6 7)) '(Y X X  X Y Y  X Y Y))))

(ert-deftest test-oll-uf-p ()
  "Test of `oll-uf-p'"
  (should (oll-uf-p 2 '(1 2 3)))
  (should-not (oll-uf-p 4 '(1 2 3))))

(ert-deftest test-oll-ufs ()
  "Test of `oll-ufs'"
  (should (equal (oll-ufs '(2 3 5 6 7)) '(nil t t nil t t t nil nil)))
  (should (equal (oll-ufs '(2 3 5 6 7) 1) '(nil t t nil t t t nil nil))))

(ert-deftest test-oll-rotation-number ()
  "Test of `oll-rotation-number'"
 (should (equal (mapcar #'oll-rotation-number '(nil y y2 yw)) '(0 1 2 3))))

(provide 'test-oll.el)
