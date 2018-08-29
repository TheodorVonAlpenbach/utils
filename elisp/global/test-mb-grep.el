(require 'ert)
(require 'mb-grep)

(ert-deftest test-mb-grep-dirs ()
  "Test of `mb-grep-dirs'.
We could make this test a bit faster by let-ing the calls to
mb-grep-dirs."
  (should (> (length (mb-grep-dirs nil "/home/mbe/")) 100))
  (should (equal (length (mb-grep-dirs nil "/home/mbe/"))
  		 (length (mb-grep-dirs 0 "/home/mbe/"))))
  (should (> (length (mb-grep-dirs 1 "/home/mbe/"))
  	     (length (mb-grep-dirs nil "/home/mbe/"))))
  (should-not (find "/home/" (mb-grep-dirs nil "/home/mbe/") :test #'string=))
  (should (find "/home/" (mb-grep-dirs 1 "/home/mbe/") :test #'string=))
  (should-not (find "/" (mb-grep-dirs 1 "/home/mbe/") :test #'string=))

  (should (equal (mb-grep-dirs 10 "/home/mbe/") '("/home/mbe/" "/home/")))
  (should (find "/home/" (mb-grep-dirs 11 "/home/mbe/") :test #'string=))
  (should (find "/home/mbe" (mb-grep-dirs 11 "/home/mbe/") :test #'string=))
  ;; minus
  (should (equal (mb-grep-dirs "-" "/home/mbe/") '("/home/mbe/")))
  (should (find "/home/mbe/" (mb-grep-dirs -1 "/home/mbe/") :test #'string=))
  (should (find "/home/mbe/.emacs.d" (mb-grep-dirs -1 "/home/mbe/") :test #'string=)))
;;(equal (mb-grep-dirs 1 "/home/mbe/") "/home/")

(provide 'test-mb-grep)
