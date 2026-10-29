;;; test-mb-sql.el --- Tests for mb-sql -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mb-sql)

(ert-deftest mb-sql-sends-current-line ()
  (with-temp-buffer
    (insert "  select * from mytable;  \nignored")
    (goto-char (point-min))
    (let (sent)
      (cl-letf (((symbol-function 'sql-send-string)
                 (lambda (string)
                   (setq sent string))))
        (mb-sql-send-expression)
        (should (equal sent "\nselect * from mytable;"))))))

(ert-deftest mb-sql-strips-only-leading-comment-marker ()
  (with-temp-buffer
    (insert "-- select '--literal' as value; -- trailing comment")
    (let (sent)
      (cl-letf (((symbol-function 'sql-send-string)
                 (lambda (string)
                   (setq sent string))))
        (mb-sql-send-expression)
        (should
         (equal sent
                "\nselect '--literal' as value; -- trailing comment"))))))

(ert-deftest mb-sql-rejects-empty-expression ()
  (with-temp-buffer
    (insert "  --   ")
    (should-error (mb-sql-send-expression) :type 'user-error)))

(ert-deftest mb-sql-mode-setup-is-local ()
  (let ((global-binding (lookup-key sql-mode-map (kbd "C-c C-e"))))
    (with-temp-buffer
      (sql-mode)
      (should (eq (local-key-binding (kbd "C-c C-e"))
                  #'mb-sql-send-expression)))
    (should (equal (lookup-key sql-mode-map (kbd "C-c C-e"))
                   global-binding))))

(provide 'test-mb-sql)

;;; test-mb-sql.el ends here
