;;; test-mb-octave-eval.el --- Tests for mb-octave-eval -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mb-octave-eval)

(ert-deftest mb-octave-source-buffer-quotes-filename ()
  (let ((buffer (generate-new-buffer " *mb-octave-source*"))
        sent)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq buffer-file-name "/tmp/my \"function\".m"))
          (cl-letf (((symbol-function 'inferior-octave-send-list-and-digest)
                     (lambda (commands)
                       (setq sent commands))))
            (octave-source-buffer t buffer))
          (should
           (equal sent
                  '("source (\"/tmp/my \\\"function\\\".m\");\n"))))
      (kill-buffer buffer))))

(ert-deftest mb-octave-last-sexp-handles-code-and-comments ()
  (with-temp-buffer
    (insert "value = foo (1, 2)")
    (should (equal (octave-last-sexp) "foo (1, 2)")))
  (with-temp-buffer
    (insert "## foo (1, 2)")
    (should (equal (octave-last-sexp) "foo (1, 2)"))))

(ert-deftest mb-octave-send-string-returns-and-inserts-output ()
  (let ((inferior-buffer (generate-new-buffer " *mb-octave-process*"))
        (inferior-octave-process 'process)
        (inferior-octave-output-list nil)
        (inferior-octave-output-string "")
        (inferior-octave-receive-in-progress nil)
        (inferior-octave-buffer nil)
        (octave-send-echo-input t)
        (octave-send-show-buffer nil)
        sent)
    (setq inferior-octave-buffer inferior-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'inferior-octave)
                   #'ignore)
                  ((symbol-function 'compilation-forget-errors)
                   #'ignore)
                  ((symbol-function 'inferior-octave-send-list-and-digest)
                   (lambda (commands)
                     (push (car commands) sent)
                     (setq inferior-octave-output-list
                           (list (format "out:%s"
                                         (string-trim (car commands))))
                           inferior-octave-receive-in-progress nil))))
          (should (equal (octave-send-string "a\nb" t)
                         "out:a\nout:b"))
          (should (equal (nreverse sent) '("a\n" "b\n")))
          (with-current-buffer inferior-buffer
            (should (equal (buffer-string) "a\nout:ab\nout:b"))))
      (kill-buffer inferior-buffer))))

(ert-deftest mb-octave-wait-times-out ()
  (let ((inferior-octave-receive-in-progress t)
        (mb-octave-response-timeout 0))
    (should-error (mb-octave--wait-for-response 'process)
                  :type 'error)))

(ert-deftest mb-octave-eval-current-line-normalizes-comment ()
  (with-temp-buffer
    (insert "  ## ones (2)  ")
    (let (sent)
      (cl-letf (((symbol-function 'octave-send-string)
                 (lambda (string &optional print-repl-p)
                   (setq sent (list string print-repl-p)))))
        (octave-eval-current-line)
        (should (equal sent '("\nones (2)" t)))
        (should (equal octave-last-debug-expression "\nones (2)"))))))

(ert-deftest mb-octave-eval-last-sexp-returns-output ()
  (with-temp-buffer
    (insert "foo (1)")
    (cl-letf (((symbol-function 'octave-send-string)
               (lambda (_string &optional _print-repl-p)
                 "42"))
              ((symbol-function 'message) #'ignore))
      (should (equal (octave-eval-last-sexp) "42")))
    (let ((current-prefix-arg '(4)))
      (should-error (octave-eval-last-sexp) :type 'user-error))))

(provide 'test-mb-octave-eval)

;;; test-mb-octave-eval.el ends here
