(require 'ert)
(require 'mb-grep)

(ert-deftest test-mb-grep-dirs ()
  "Test of `mb-grep-dirs'.
We could make this test a bit faster by let-ing the calls to
mb-grep-dirs."
  (let* ((dir (expand-directory-name +mb-lisp-dir+))
	 (pdir (expand-directory-name ".." dir)))
    (should (> (length (mb-grep-dirs nil dir)) 20))
    (should (equal (length (mb-grep-dirs nil dir))
  		   (length (mb-grep-dirs 0 dir))))
    (should (> (length (mb-grep-dirs 1 dir))
  	       (length (mb-grep-dirs nil dir))))
    (should (cl-find pdir (mb-grep-dirs 1 dir) :test #'string=))
    (should-not (cl-find "/" (mb-grep-dirs 1 dir) :test #'string=))
    (should (equal (mb-grep-dirs 10 dir)
		   (list (expand-directory-name ".." dir))))
    (should (cl-find pdir (mb-grep-dirs 11 dir) :test #'string=))
    (should (cl-find dir (mb-grep-dirs 11 dir) :test #'string=))
    ;; minus
    (should (equal (mb-grep-dirs "-" dir) (list dir)))
    (should (cl-find dir (mb-grep-dirs -1 dir) :test #'string=))
    (should (cl-find (expand-directory-name "~/.emacs.d/")
		  (mb-grep-dirs -1 (expand-directory-name "~/"))
		  :test #'string=))))

(provide 'test-mb-grep)
