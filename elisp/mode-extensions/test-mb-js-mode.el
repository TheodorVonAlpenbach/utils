;;; test-mb-js-mode.el --- Tests for mb-js-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-js-mode)

(ert-deftest mb-js-mode-local-settings ()
  (with-temp-buffer
    (js-mode)
    (should (= js-indent-level 4))
    (should (= js-switch-indent-offset 4))
    (should (= tab-width 4))
    (should-not indent-tabs-mode)
    (should-not parens-require-spaces)
    (should electric-pair-mode)
    (mb-js-mode-hook-function)
    (should electric-pair-mode)))

(ert-deftest mb-js-mode-nodejs-repl-hook ()
  (should (memq #'mb-nodejs-repl-mode-hook-function
                nodejs-repl-mode-hook))
  (with-temp-buffer
    (setq-local parens-require-spaces t)
    (mb-nodejs-repl-mode-hook-function)
    (should-not parens-require-spaces)))

(ert-deftest mb-js-mode-repl-buffer-lookup ()
  (let ((mb-js-nodejs-repl-process-name "test-node"))
    (should (equal (nodejs-repl-buffer-name) "*test-node*"))
    (should-not (nodejs-repl-buffer))
    (let ((buffer (get-buffer-create "*test-node*")))
      (unwind-protect
          (should (eq (nodejs-repl-buffer) buffer))
        (kill-buffer buffer)))))

(ert-deftest mb-js-mode-evaluation ()
  (let (displayed)
    (cl-letf (((symbol-function 'mb-js--nodejs-execute)
               (lambda (string)
                 (format "result:%s" string)))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq displayed (apply #'format format-string arguments)))))
      (should (equal (js-eval-string "2+2") "result:2+2"))
      (should-not displayed)
      (should (equal (js-eval-string "[1,2]" t) "result:[1,2]"))
      (should (equal displayed "result:[1,2]"))
      (should-error (js-eval-string-raw 42)
                    :type 'wrong-type-argument))))

(ert-deftest mb-js-mode-missing-nodejs-repl ()
  (cl-letf (((symbol-function 'require)
             (lambda (feature &optional _filename _noerror)
               (unless (eq feature 'nodejs-repl)
                 (error "Unexpected feature: %S" feature)))))
    (should-error (mb-js--nodejs-execute "2+2") :type 'user-error)))

(provide 'test-mb-js-mode)

;;; test-mb-js-mode.el ends here
