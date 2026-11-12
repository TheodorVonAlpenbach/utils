;;; test-mb-tex.el --- Tests for mb-tex -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(unless (require 'latex nil t)
  (defvar TeX-expand-list nil)
  (fset 'TeX-run-function (lambda (&rest arguments) arguments))
  (provide 'latex))

(defconst mb-tex-test--auctex-run-function
  (symbol-function 'TeX-run-function))

(require 'mb-tex)

(ert-deftest mb-tex-preserves-auctex-run-function ()
  (should (eq (symbol-function 'TeX-run-function)
	      mb-tex-test--auctex-run-function))
  (should-not (fboundp 'mb-open-pdf))
  (should-not (fboundp 'mb-describe-mode)))

(ert-deftest mb-tex-master-prefix ()
  (cl-letf (((symbol-function 'TeX-master-file)
	     (lambda () "score.tex"))
	    ((symbol-function 'TeX-master-directory)
	     (lambda () "/tmp/project/")))
    (should (equal (master-prefix)
		   "/tmp/project/score"))))

(provide 'test-mb-tex)

;;; test-mb-tex.el ends here
