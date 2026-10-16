;;; test-mb-python.el --- Tests for mb-python -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-python)

(ert-deftest mb-python-local-settings ()
  (with-temp-buffer
    (python-mode)
    (should-not parens-require-spaces)
    (should (local-variable-p 'evil-symbol-word-search))
    (should (symbol-value 'evil-symbol-word-search))
    (should (symbol-value 'display-line-numbers-mode))
    (set-mb-python-locals)
    (should (symbol-value 'display-line-numbers-mode))))

(ert-deftest mb-python-last-expression-bounds ()
  (dolist (case '(("result = foo(1, 2)" "foo(1, 2)")
                  ("value = items[-1]" "items[-1]")
                  ("value = [1, 2]" "[1, 2]")))
    (with-temp-buffer
      (insert (car case))
      (python-mode)
      (goto-char (point-max))
      (let ((origin (point))
            (bounds (mb-python-last-sexp-region)))
        (should (= (point) origin))
        (should (equal (buffer-substring-no-properties
                        (car bounds) (cadr bounds))
                       (cadr case))))))
  (with-temp-buffer
    (python-mode)
    (should-error (mb-python-last-sexp-region) :type 'user-error)))

(ert-deftest mb-python-defun-bounds ()
  (with-temp-buffer
    (insert "def outer():\n"
            "    value = 1\n"
            "    def inner():\n"
            "        return value\n"
            "    return inner()\n"
            "\n"
            "result = outer()\n")
    (python-mode)
    (goto-char (point-min))
    (search-forward "return value")
    (pcase-let ((`(,start ,end) (mb-python-defun-region)))
      (should (equal (buffer-substring-no-properties start end)
                     "    def inner():\n        return value\n")))
    (goto-char (point-min))
    (search-forward "value = 1")
    (pcase-let ((`(,start ,end) (mb-python-defun-region)))
      (should (equal (buffer-substring-no-properties start end)
                     (concat "def outer():\n"
                             "    value = 1\n"
                             "    def inner():\n"
                             "        return value\n"
                             "    return inner()\n"))))
    (goto-char (point-max))
    (should-error (mb-python-defun-region) :type 'user-error)))

(provide 'test-mb-python)

;;; test-mb-python.el ends here
