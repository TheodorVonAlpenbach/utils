(require 'ert)
(require 'oll)

(ert-deftest test-oll-expand-corners ()
  "Test of `oll-expand-corners'"
  (should (equal (oll-expand-corners '(3 4 5 6 9) '(r l)) '(r u u l)))
  (should (equal (oll-expand-corners '(5 9) '(r r r)) '(r r u r)))
  (should (equal (oll-expand-corners '(2 3 4 5 6 8 9) '(r l)) '(r u u l))))

(ert-deftest test-oll-uf-p ()
  "Test of `oll-uf-p'"
  (should (oll-uf-p 2 '(1 2 3)))
  (should-not (oll-uf-p 4 '(1 2 3))))

(ert-deftest test-oll-ufs ()
  "Test of `oll-ufs'"
  (should (equal (oll-ufs '(2 3 5 6 7)) '(nil t t nil t t t nil nil)))
  (should (equal (oll-ufs '(5 9)) '(nil nil nil  nil t nil  nil nil t)))
  (should (equal (oll-ufs '(2 3 4 5 6 8 9)) '(nil t t  t t t  nil t t))))

(ert-deftest test-oll-sides ()
  "Test of `oll-sides'"
 (should (equal (oll-sides '(5 9) '(r r r)) '((Y Y X) (Y Y X) (X Y X) (Y Y X))))
 (should (equal (oll-sides '(2 3 4 5 6 8 9) '(r l)) '((Y X X) (X X X) (X X Y) (X X X)))))

(provide 'test-oll.el)
