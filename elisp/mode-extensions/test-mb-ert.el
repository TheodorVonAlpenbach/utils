;;; test-mb-ert.el --- Tests for mb-ert -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-ert)

(ert-deftest test-mb-ert-test-buffer-p ()
  "Test of `mb-ert-test-buffer-p'"
  (should (let ((buffer (get-buffer-create "test-abcdefeuropris.el")))
	    (prog1 (mb-ert-test-buffer-p buffer)
	      (kill-buffer buffer)))))

(ert-deftest test-mb-ert-swap-defun-name ()
  "Test of `mb-ert-swap-defun-name'"
  (should (equal (mb-ert-swap-defun-name "qwe") "test-qwe"))
  (should (equal (mb-ert-swap-defun-name "test-qwe") "qwe"))
  (should (equal (mb-ert-swap-defun-name "test-qwe" :ert) "test-test-qwe"))
  (should (equal (mb-ert-swap-defun-name "12345qwe" :non-ert) "qwe"))
  (should (equal (mb-ert-swap-defun-name "qwe" :ert "supertest-") "supertest-qwe"))
  (should (equal (mb-ert-swap-defun-name "qwe" :ert-soft) "test-qwe"))
  (should (equal (mb-ert-swap-defun-name "test-qwe" :ert-soft) "test-qwe"))
  (should-error (mb-ert-swap-defun-name 'qwe)))

(ert-deftest test-mb-ert-swap-filename ()
  "Test of `mb-ert-swap-filename'"
  (should (equal (mb-ert-swap-filename "/qwe/qwe.el") "/qwe/test-qwe.el"))
  (should (equal (mb-ert-swap-filename "/qwe/test-qwe.el") "/qwe/qwe.el")))

(ert-deftest test-mb-ert-get-test-filename ()
  "Test of `mb-ert-get-test-filename'"
  (should (equal (mb-ert-get-test-filename "/qwe/qwe.el") "/qwe/test-qwe.el"))
  (should (equal (mb-ert-get-test-filename "/qwe/test-qwe.el") "/qwe/test-qwe.el")))

(ert-deftest test-mb-ert-test-filename-p ()
  "Test of `mb-ert-test-filename-p'"
  (should (mb-ert-test-filename-p "~/path/test-qwe.el"))
  (should-not (mb-ert-test-filename-p "~/path/qwe.el"))
  (should-not (mb-ert-test-filename-p "~/projects/utils/elisp/mode-extensions/mb-js-mode.el"))
  (should (mb-ert-test-filename-p
           "~/projects/utils/elisp/mode-extensions/test-mb-js-mode.el")))

(ert-deftest test-mb-ert-name-p ()
  "Test of `mb-ert-name-p'"
  (should (mb-ert-name-p "test-qwe"))
  (should (equal (mb-ert-name-p "atest-qwe") nil))
  (should (equal (mb-ert-name-p "test-qwe" "asd") nil))
  (should (mb-ert-name-p "atest-qwe" "atest")))

(ert-deftest test-mb-ert-file-defuns ()
  "Test of `mb-ert-file-defuns'"
  (should-error (mb-ert-file-defuns "~/projects/utils/elisp/utils/mb-sequences.el"))
  (let ((file (make-temp-file "test-mb-ert-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(ert-deftest test-one () t)\n"
                    "(ert-deftest test-two () t)\n"
                    "(ert-deftest test-one () t)\n"))
          (should (equal (mb-ert-file-defuns file)
                         '(test-one test-two))))
      (delete-file file))))

(ert-deftest test-mb-ert-swap-defun-symbol ()
  "Test of `mb-ert-swap-defun-symbol'"
  (should (equal (mb-ert-swap-defun-symbol 'qwe) 'test-qwe))
  (should (equal (mb-ert-swap-defun-symbol 'qwe) 'test-qwe)))

(ert-deftest test-mb-ert-definition-navigation ()
  "Test definition detection and navigation without startup helpers."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun first-function () nil)\n\n"
            "(ert-deftest test-second-function () t)\n\n"
            "(defmacro third-macro () nil)\n")
    (goto-char (point-min))
    (search-forward "nil")
    (should (eq (mb-ert--defun-symbol-at-point) 'first-function))
    (should (mb-ert--goto-definition "test-second-function"))
    (should (looking-at "(ert-deftest test-second-function"))
    (should (mb-ert--goto-definition "third-macro"))
    (should (looking-at "(defmacro third-macro"))))

(ert-deftest test-mb-ert-map-includes-region-command ()
  (should (eq (lookup-key (mb-ert-map) "r") #'mb-ert-test-region)))

(ert-deftest test-mb-ert-region-defuns ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(ert-deftest test-one () t)\n"
            "(defun helper () nil)\n"
            "(ert-deftest test-two () t)\n")
    (let ((end (save-excursion
                 (goto-char (point-min))
                 (forward-line 2)
                 (point))))
      (should (equal (mb-ert--buffer-defuns (point-min) end)
                     '(test-one))))))

(ert-deftest test-mb-ert-test-symbol-selection ()
  (let (selector)
    (cl-letf (((symbol-function 'ert)
               (lambda (value) (setq selector value))))
      (should (mb-ert-test-symbols '(test-one test-one test-two)))
      (should (equal selector '(member test-one test-two)))
      (setq selector nil)
      (mb-test-ert-symbols 'test-three)
      (should (equal selector '(member test-three)))))
  (should-error (mb-ert-test-symbols nil) :type 'user-error)
  (should-error (mb-ert-test-symbols '(test-one "test-two"))
                :type 'user-error))

(ert-deftest test-mb-ert-region-command-selects-tests ()
  (with-temp-buffer
    (insert "(ert-deftest test-one () t)\n"
            "(ert-deftest test-two () t)\n")
    (let (symbols)
      (cl-letf (((symbol-function 'mb-ert-test-symbols)
                 (lambda (value) (setq symbols value))))
        (mb-ert-test-region (point-min) (point-max)))
      (should (equal symbols '(test-one test-two))))))

(ert-deftest test-mb-ert-invalid-targets-report-user-errors ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should-error (mb-ert-test-defun) :type 'user-error)
    (should-error (mb-ert-test-buffer) :type 'user-error)
    (should-error (mb-ert-get-test-filename (current-buffer))
                  :type 'user-error))
  (should-error (mb-eval/load "/path/that/does/not/exist.el")
                :type 'user-error))

(provide 'test-mb-ert)

;;; test-mb-ert.el ends here
