;;; test-mb-sexp.el --- Tests for mb-sexp -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'mb-sexp)

(ert-deftest test-mb-sexp-compatibility-aliases ()
  "Legacy public names resolve to the namespaced implementations."
  (dolist (pair '((goto-first-sexp . mb-sexp-goto-first)
                  (count-sexps-current-level . mb-sexp-count-current-level)
                  (count-sexps-region . mb-sexp-count-region)
                  (lift-sexp . mb-sexp-lift)
                  (defun-symbol . mb-sexp-defun-symbol)
                  (in-defun-p . mb-sexp-in-defun-p)
                  (defun-symbols . mb-sexp-defun-symbols)))
    (should (eq (indirect-function (car pair))
                (indirect-function (cdr pair)))))
  (should (eq +defun-symbols+ mb-sexp-defining-forms))
  (should (eq +defun-regexp+ mb-sexp-defining-form-regexp)))

(ert-deftest test-mb-sexp-goto-first ()
  "Move to the first sexp at the current nesting level."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(outer (a b c))")
    (search-backward "c")
    (should (= (mb-sexp-goto-first) 9))
    (should (looking-at "a"))))

(ert-deftest test-mb-sexp-count-current-level ()
  "Count sexps at the current level without printing from the helper."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(outer (a b (c d)))")
    (search-backward "b")
    (should (= (mb-sexp-count-current-level) 3))))

(ert-deftest test-mb-sexp-count-region ()
  "Count complete top-level sexps in a region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "a  b\n(c d)\n")
    (should (= (mb-sexp-count-region (point-min) (point-max)) 3))
    (should (= (mb-sexp-count-region (point-min) (point-min)) 0)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "a (unfinished")
    (should (= (mb-sexp-count-region (point-min) (point-max)) 1))))

(ert-deftest test-mb-sexp-lift-positive ()
  "Positive lifting delegates to standard `raise-sexp' behavior."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a b (c d))")
    (search-backward "c")
    (mb-sexp-lift 1)
    (should (string= (buffer-string) "(a b c)"))))

(ert-deftest test-mb-sexp-lift-zero ()
  "Zero lifting retains the legacy delimiter-splicing behavior."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a b (c d))")
    (search-backward "c")
    (mb-sexp-lift 0)
    (should (string= (buffer-string) "(a b c d)"))))

(ert-deftest test-mb-sexp-lift-rejects-negative-count ()
  "Negative lift counts are rejected explicitly."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a b)")
    (goto-char 2)
    (should-error (mb-sexp-lift -1) :type 'user-error)))

(ert-deftest test-mb-sexp-defun-symbol ()
  "Recognize definition names at the opening delimiter and inside the body."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun alpha (x)\n  x)\n")
    (goto-char (point-min))
    (should (eq (mb-sexp-defun-symbol) 'alpha))
    (search-forward "x)")
    (should (eq (mb-sexp-defun-symbol) 'alpha))
    (should (mb-sexp-in-defun-p))
    (goto-char (point-max))
    (should-not (mb-sexp-defun-symbol))
    (should-not (mb-sexp-in-defun-p))))

(ert-deftest test-mb-sexp-defun-symbol-honors-position-and-forms ()
  "POSITION and the defining-form list affect recognition."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun first () 1)\n(my-def second () 2)\n")
    (let ((first-position 10)
          (second-position (progn (search-backward "second") (point))))
      (goto-char (point-max))
      (should (eq (mb-sexp-defun-symbol first-position) 'first))
      (should-not (mb-sexp-defun-symbol second-position))
      (should (eq (mb-sexp-defun-symbol second-position '(my-def))
                  'second)))))

(ert-deftest test-mb-sexp-defun-symbols-honors-buffer ()
  "Definition discovery uses BUFFER and ignores comments and strings."
  (let ((source (generate-new-buffer " *mb-sexp-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (emacs-lisp-mode)
            (insert
             (concat ";; (defun fake () nil)\n"
                     "(defun one () 1)\n"
                     "\"(defun not-real () nil)\"\n"
                     "(cl-defun two () 2)\n"
                     "(defvar ignored 3)\n"
                     "(ert-deftest test-three () t)\n")))
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert "(defun current () nil)")
            (should (equal (mb-sexp-defun-symbols source)
                           '(one two test-three)))))
      (kill-buffer source))))

(ert-deftest test-mb-sexp-insert-defun-symbols ()
  "Insert the printed list of definitions from the requested buffer."
  (let ((source (generate-new-buffer " *mb-sexp-insert-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (emacs-lisp-mode)
            (insert "(defun one () 1)\n(defun two () 2)\n"))
          (with-temp-buffer
            (insert-defun-symbols source)
            (should (string= (buffer-string) "(one two)"))))
      (kill-buffer source))))

(provide 'test-mb-sexp)

;;; test-mb-sexp.el ends here
