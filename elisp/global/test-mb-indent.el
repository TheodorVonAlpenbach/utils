;;; test-mb-indent.el --- Tests for mb-indent -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'mb-indent)

(ert-deftest mb-indent-registers-single-and-list-specifications ()
  (let ((first (make-symbol "mb-indent-first"))
        (second (make-symbol "mb-indent-second"))
        (reference (make-symbol "mb-indent-reference")))
    (put reference 'common-lisp-indent-function '(4 &body))
    (set-indent first 2)
    (set-indent (list second) reference)
    (should (equal (get first 'common-lisp-indent-function) 2))
    (should (equal (get second 'common-lisp-indent-function)
                   '(4 &body)))))

(ert-deftest mb-indent-registers-retained-metadata ()
  (should
   (equal (get 'aif 'common-lisp-indent-function)
          (get 'prog1 'common-lisp-indent-function)))
  (should
   (equal (get 'concat 'common-lisp-indent-function)
          (get 'progn 'common-lisp-indent-function)))
  (should (equal (get 'for 'common-lisp-indent-function) 3))
  (should (equal (get 'handler-bind 'common-lisp-indent-function)
                 '((&whole 4 &rest 1) 2 &body))))

(ert-deftest mb-indent-retains-upstream-loop-implementation ()
  (should-not (fboundp 'mb-common-lisp-indent-function))
  (should
   (string-match-p
    "cl-indent"
    (or (symbol-file 'common-lisp-loop-part-indentation 'defun) ""))))

(ert-deftest mb-indent-configures-common-lisp-indentation ()
  (should (eq (default-value 'lisp-indent-function)
              #'common-lisp-indent-function))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(aif condition\nbody\nfallback)")
    (indent-region (point-min) (point-max))
    (should
     (equal (buffer-string)
            "(aif condition\n  body\n  fallback)"))))

(provide 'test-mb-indent)

;;; test-mb-indent.el ends here
