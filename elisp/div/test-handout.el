(require 'ert)
(require 'handout)

(ert-deftest test-ho-pop-hidden ()
  "Test of `ho-pop-hidden'"
  (let ((hand (ho-make-hand (0-n 3))))
    (should (equal (ho-make-hand (0-n 3)) (list nil '(0 1 2))))

    (should (equal (ho-pop-visible hand) nil))
    (should (equal (ho-visible hand) nil))
    (should (equal (ho-hidden hand) (0-n 3)))

    (should (equal (ho-pop-hidden hand) 0))
    (should (equal (ho-visible hand) '(0)))
    (should (equal (ho-hidden hand) '(1 2)))

    (should (equal (ho-pop-hidden hand) 1))
    (should (equal (ho-visible hand) '(1 0)))
    (should (equal (ho-hidden hand) '(2)))

    (should (equal (ho-pop-hidden hand) 2))
    (should (equal (ho-visible hand) '(2 1 0)))
    (should (equal (ho-hidden hand) nil))

    (should (equal (ho-pop-hidden hand) 0))
    (should (equal (ho-visible hand) '(0)))
    (should (equal (ho-hidden hand) '(1 2)))

    (should (equal (ho-pop-hidden hand) 1))
    (should (equal (ho-pop-hidden hand) 2))

    (should (equal (ho-pop-visible hand) 2))
    (should (equal (ho-pop-visible hand) 1))
    (should (equal (ho-pop-visible hand) 0))
    (should (equal (ho-pop-visible hand) nil))
    (should (equal (ho-pop-hidden hand) nil))))

(provide 'test-handout)
