(require 'ert)
(require 'lsconf-sensors)

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
  (should-not (mb-ert-test-filename-p "~/path/qwe.el")))

(ert-deftest test-mb-ert-name-p ()
  "Test of `mb-ert-name-p'"
  (should (mb-ert-name-p "test-qwe"))
  (should (equal (mb-ert-name-p "atest-qwe") nil))
  (should (equal (mb-ert-name-p "test-qwe" "asd") nil))
  (should (mb-ert-name-p "atest-qwe" "atest")))

(provide 'test-mb-ert.el)

