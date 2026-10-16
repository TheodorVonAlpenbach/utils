;;; test-mb-table.el --- Tests for mb-table -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-table)

(ert-deftest mb-table-point-to-line ()
  (with-temp-buffer
    (insert "one\ntwo\nthree")
    (should (= (point-2-line 1) 1))
    (should (= (point-2-line 6) 2))
    (goto-char 10)
    (should (= (point-2-line) 3))))

(ert-deftest mb-table-item-lengths ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "alpha (beta gamma) 3\nx y 444\n")
    (should (equal (table-item-lengths-line 1) '(5 12 1)))
    (should (equal (table-collect-sexp-lengths 1 2)
                   '((5 12 1) (1 1 3))))))

(ert-deftest mb-table-clean ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "alpha (beta gamma) 3\nx y 444\n")
    (table-clean (point-min) (point-max))
    (should
     (equal (buffer-string)
            "alpha  (beta gamma)  3\nx      y             444\n"))))

(ert-deftest mb-table-clean-selected-lines ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "untouched\nlong 1\nx 22\nuntouched\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((beg (point)))
      (forward-line 2)
      (table-clean beg (point)))
    (should (equal (buffer-string)
                   "untouched\nlong  1\nx     22\nuntouched\n"))))

(ert-deftest mb-table-clean-line ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert " a   (b c)  10")
    (table-clean-line 1 '(0 5 14))
    (should (equal (buffer-string) "a    (b c)    10"))))

(ert-deftest mb-table-clean-paragraph ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "a 1\nlong 22\n\noutside")
    (goto-char 2)
    (table-clean-paragraph)
    (should (equal (buffer-string) "a     1\nlong  22\n\noutside"))))

(ert-deftest mb-table-validation ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "a 1\nb\n")
    (should-error (table-clean (point-min) (point-max))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(unfinished\n")
    (should-error (table-item-lengths-line 1)))
  (with-temp-buffer
    (should-error (table-clean-line 1 '(0))))
  (with-temp-buffer
    (should-error (table-collect-sexp-lengths 2 1))))

(provide 'test-mb-table)

;;; test-mb-table.el ends here
