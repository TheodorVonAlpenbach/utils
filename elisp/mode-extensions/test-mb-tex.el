;;; test-mb-tex.el --- Tests for mb-tex -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(unless (require 'latex nil t)
  (defvar TeX-expand-list nil)
  (defvar LaTeX-mode-hook nil)
  (fset 'TeX-run-function (lambda (&rest arguments) arguments))
  (provide 'latex))

(defconst mb-tex-test--auctex-run-function
  (symbol-function 'TeX-run-function))

(defconst mb-tex-test--latex-mode-hook-before-load
  (copy-sequence LaTeX-mode-hook))

(defconst mb-tex-test--expand-list-before-load
  (copy-tree TeX-expand-list))

(require 'mb-tex)

(defconst mb-tex-test--latex-mode-hook-after-load
  (copy-sequence LaTeX-mode-hook))

(defconst mb-tex-test--expand-list-after-load
  (copy-tree TeX-expand-list))

(ert-deftest mb-tex-load-has-no-global-effects ()
  (should (equal mb-tex-test--latex-mode-hook-after-load
		 mb-tex-test--latex-mode-hook-before-load))
  (should (equal mb-tex-test--expand-list-after-load
		 mb-tex-test--expand-list-before-load))
  (should-not
   (advice-member-p #'mode-help-follow-mode-advice #'describe-mode)))

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

(ert-deftest mb-tex-setup-is-idempotent ()
  (remove-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (setq TeX-expand-list
	(cl-delete mb-tex--viewer-expansion TeX-expand-list :test #'equal))
  (advice-remove #'describe-mode #'mode-help-follow-mode-advice)
  (mb-tex-setup)
  (mb-tex-setup)
  (should TeX-auto-save)
  (should TeX-parse-self)
  (should (equal TeX-electric-math '(?$ . ?$)))
  (should reftex-plug-into-AUCTeX)
  (should (equal reftex-insert-label-flags '("s" "sfte")))
  (should (memq #'turn-on-reftex LaTeX-mode-hook))
  (should (= (cl-count mb-tex--viewer-expansion TeX-expand-list
		       :test #'equal)
	     1))
  (should
   (advice-member-p #'mode-help-follow-mode-advice #'describe-mode)))

(provide 'test-mb-tex)

;;; test-mb-tex.el ends here
