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
  (should-error (mb-ert-swap-defun-name 'qwe)))

(ert-deftest test-mb-ert-swap-filename ()
  "Test of `mb-ert-swap-filename'"
  (should (equal (mb-ert-swap-filename "/qwe/qwe.el") "/qwe/test-qwe.el"))
  (should (equal (mb-ert-swap-filename "/qwe/test-qwe.el") "/qwe/qwe.el"))
  (let ((test-buffer (get-buffer-create "test-abcdefeuropris.el"))
	(buffer (get-buffer-create "abcdefeuropris.el")))
    (should (equal (mb-ert-swap-filename test-buffer)
		   (expand-file-name "abcdefeuropris.el" (buffer-directory))))
    (kill-buffer test-buffer)
    (kill-buffer buffer)))

(ert-deftest test-mb-ert-get-test-filename ()
  "Test of `mb-ert-get-test-filename'"
  (should (equal (mb-ert-get-test-filename "/qwe/qwe.el") "/qwe/test-qwe.el"))
  (should (equal (mb-ert-get-test-filename "/qwe/test-qwe.el") "/qwe/test-qwe.el")))

(ert-deftest test-mb-ert-test-filename-p ()
  "Test of `mb-ert-test-filename-p'"
  (should (mb-ert-test-filename-p "~/path/test-qwe.el"))
  (should-not (mb-ert-test-filename-p "~/path/qwe.el")))

(ert-deftest test-mb-swap-ert-defun ()
"Test of `mb-swap-ert-defun'"
 (should (equal (mb-swap-ert-defun ) nil)))

(provide 'test-mb-ert.el)
