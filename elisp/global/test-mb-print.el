;;; test-mb-print.el --- Tests for mb-print -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-print)

(ert-deftest test-mb-print-compatibility-aliases ()
  "Legacy Notepad command names resolve to namespaced implementations."
  (should (eq (indirect-function 'print-notepad-file)
              (indirect-function 'mb-print-file)))
  (should (eq (indirect-function 'print-notepad-region)
              (indirect-function 'mb-print-region)))
  (should (eq (indirect-function 'print-notepad-buffer)
              (indirect-function 'mb-print-buffer))))

(ert-deftest test-mb-print-file-invokes-configured-program ()
  "A readable file is passed after the configured program arguments."
  (let ((file (make-temp-file "mb-print-test-"))
        (mb-print-program "mock-notepad")
        (mb-print-program-arguments '("/p" "/quiet"))
        (mb-print-output-buffer "*mb-print-test-output*")
        invocation)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program)
                     (should (string= program "mock-notepad"))
                     "/mock/notepad"))
                  ((symbol-function 'call-process)
                   (lambda (&rest arguments)
                     (setq invocation arguments)
                     0)))
          (should (= (mb-print-file file) 0))
          (should
           (equal invocation
                  (list "/mock/notepad" nil "*mb-print-test-output*" nil
                        "/p" "/quiet" (expand-file-name file)))))
      (delete-file file)
      (when (get-buffer mb-print-output-buffer)
        (kill-buffer mb-print-output-buffer)))))

(ert-deftest test-mb-print-file-requires-readable-file ()
  "An unreadable input file is reported before starting a process."
  (let ((missing (make-temp-name
                  (expand-file-name "mb-print-missing-"
                                    temporary-file-directory))))
    (should-error (mb-print-file missing) :type 'user-error)))

(ert-deftest test-mb-print-file-requires-program ()
  "A missing print executable is reported explicitly."
  (let ((file (make-temp-file "mb-print-test-"))
        (mb-print-program "missing-print-program"))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) nil)))
          (should-error (mb-print-file file) :type 'user-error))
      (delete-file file))))

(ert-deftest test-mb-print-file-reports-process-failure ()
  "A nonzero print-process status is not treated as success."
  (let ((file (make-temp-file "mb-print-test-")))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/mock/notepad"))
                  ((symbol-function 'call-process)
                   (lambda (&rest _) 7)))
          (should-error (mb-print-file file) :type 'user-error))
      (delete-file file))))

(ert-deftest test-mb-print-region-uses-and-cleans-temporary-file ()
  "Region printing writes exact content and removes the temporary file."
  (let (temporary-path printed-content)
    (with-temp-buffer
      (insert "before PRINT after")
      (cl-letf (((symbol-function 'mb-print-file)
                 (lambda (file)
                   (setq temporary-path file
                         printed-content
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))
                   (should (file-exists-p file))
                   0)))
        (should (= (mb-print-region 8 13) 0))))
    (should (string= printed-content "PRINT"))
    (should-not (file-exists-p temporary-path))))

(ert-deftest test-mb-print-region-cleans-after-failure ()
  "Temporary files are removed when the print command signals."
  (let (temporary-path)
    (with-temp-buffer
      (insert "PRINT")
      (cl-letf (((symbol-function 'mb-print-file)
                 (lambda (file)
                   (setq temporary-path file)
                   (error "Expected test failure"))))
        (should-error (mb-print-region (point-min) (point-max)))))
    (should temporary-path)
    (should-not (file-exists-p temporary-path))))

(ert-deftest test-mb-print-buffer-dispatches-visited-file ()
  "A file-visiting buffer prints its file directly."
  (let (printed)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/visited.txt")
      (cl-letf (((symbol-function 'mb-print-file)
                 (lambda (file)
                   (setq printed file)
                   0)))
        (should (= (mb-print-buffer) 0))))
    (should (string= printed "/tmp/visited.txt"))))

(ert-deftest test-mb-print-buffer-dispatches-unvisited-content ()
  "A non-file buffer prints its complete region."
  (let (bounds content)
    (with-temp-buffer
      (insert "buffer content")
      (cl-letf (((symbol-function 'mb-print-region)
                 (lambda (beg end)
                   (setq bounds (list beg end)
                         content (buffer-substring-no-properties beg end))
                   0)))
        (should (= (mb-print-buffer) 0))
        (should (equal bounds (list (point-min) (point-max))))))
    (should (string= content "buffer content"))))

(provide 'test-mb-print)

;;; test-mb-print.el ends here
