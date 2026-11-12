;;; test-mb-tex.el --- Tests for mb-tex -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(unless (require 'latex nil t)
  (defvar TeX-expand-list nil)
  (defvar LaTeX-mode-hook nil)
  (defvar TeX-current-process-region-p nil)
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
		 mb-tex-test--expand-list-before-load)))

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
    (should (equal (mb-tex-master-prefix)
		   "/tmp/project/score"))))

(ert-deftest mb-tex-opens-master-pdf ()
  (let ((TeX-current-process-region-p nil)
	opened-path)
    (cl-letf (((symbol-function 'mb-tex-master-prefix)
	       (lambda () "/tmp/project/score"))
	      ((symbol-function 'file-newer-than-file-p)
	       (lambda (_tex _pdf) nil))
	      ((symbol-function 'mb-tex--display-pdf)
	       (lambda (path) (setq opened-path path))))
      (mb-tex-open-pdf))
    (should (equal opened-path "/tmp/project/score.pdf"))))

(ert-deftest mb-tex-opens-region-pdf ()
  (let ((TeX-current-process-region-p t)
	opened-path)
    (cl-letf (((symbol-function 'TeX-region-file)
	       (lambda (extension &rest _)
		 (should (equal extension "pdf"))
		 "/tmp/project/_region_.pdf"))
	      ((symbol-function 'mb-tex--display-pdf)
	       (lambda (path) (setq opened-path path))))
      (mb-tex-open-pdf))
    (should (equal opened-path "/tmp/project/_region_.pdf"))))

(ert-deftest mb-tex-displays-existing-pdf ()
  (let ((path (make-temp-file "mb-tex-" nil ".pdf"))
	opened-path
	reverted)
    (unwind-protect
	(cl-letf (((symbol-function 'find-file-noselect)
		   (lambda (file &optional _nowarn)
		     (setq opened-path file)
		     (current-buffer)))
		  ((symbol-function 'switch-to-buffer-other-window)
		   #'identity)
		  ((symbol-function 'revert-buffer)
		   (lambda (&rest _) (setq reverted t))))
	  (mb-tex--display-pdf path)
	  (should (equal opened-path path))
	  (should reverted))
      (delete-file path))))

(ert-deftest mb-tex-setup-is-idempotent ()
  (remove-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (setq TeX-expand-list
	(cl-delete mb-tex--viewer-expansion TeX-expand-list :test #'equal))
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
	     1)))

(provide 'test-mb-tex)

;;; test-mb-tex.el ends here
