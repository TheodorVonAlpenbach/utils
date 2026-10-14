;;; test-mb-utils-io.el --- Tests for mb-utils-io -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-io)

(ert-deftest test-mb-utils-io-file-round-trip ()
  "Test text file writing, appending, and reading."
  (let ((file (make-temp-file "mb-utils-io-test-")))
    (unwind-protect
        (progn
          (string-to-file "first" file)
          (string-to-file "\nsecond" file nil)
          (should (equal (file-string file) "first\nsecond"))
          (should (equal (file-lines file) '("first" "second"))))
      (delete-file file))))

(ert-deftest test-mb-utils-io-read-and-print ()
  "Test reading and printing one Lisp object."
  (let ((file (make-temp-file "mb-utils-io-test-")))
    (unwind-protect
        (progn
          (should (eq (print* '(:answer 42) file) 'finished))
          (should (equal (read* file) '(:answer 42))))
      (delete-file file))))

(ert-deftest test-mb-utils-io-temporary-file ()
  "Test portable temporary-file cleanup."
  (let (temporary-path)
    (with-temp-file*
      (setq temporary-path file)
      (should (file-exists-p file))
      (insert "temporary")
      (write-region nil nil file nil 'silent))
    (should-not (file-exists-p temporary-path))))

(ert-deftest test-mb-utils-io-temp-file-name ()
  "Test the temporary name and directory result."
  (pcase-let ((`(,name ,directory) (get-temp-file-name)))
    (should (string-prefix-p "mb-utils-" name))
    (should (file-directory-p directory))
    (should-not (file-exists-p (expand-file-name name directory)))))

(ert-deftest test-mb-utils-io-current-directory ()
  "Test temporary binding of `default-directory'."
  (let ((original default-directory))
    (should (equal (with-current-directory temporary-file-directory
                     default-directory)
                   temporary-file-directory))
    (should (equal default-directory original))))

(ert-deftest test-parse-csv-string-with-string-separator ()
  "Test simple separated text parsing."
  (should (equal (parse-csv-string "a;b\nc;d") '(("a" "b") ("c" "d"))))
  (should (equal (parse-csv-string "a;;b") '(("a" "" "b"))))
  (should (equal (parse-csv-string "a;b\n\nc;d")
                 '(("a" "b") ("c" "d")))))

(ert-deftest test-parse-csv-string-with-character-separator ()
  "Test quoted CSV parsing with a character separator."
  (should (equal (parse-csv-string "a,b\nc,d" ?,)
                 '(("a" "b") ("c" "d"))))
  (should (equal (parse-csv-string "a,\"b,c\",d" ?,)
                 '(("a" "b,c" "d"))))
  (should (equal (parse-csv-string "a,\"b\"\"c\",d" ?,)
                 '(("a" "b\"c" "d"))))
  (should (equal (parse-csv-string "a,,b" ?,) '(("a" "b"))))
  (should (equal (parse-csv-string "a,,b" ?, nil nil)
                 '(("a" "" "b"))))
  (should-error (parse-csv-string "a,\"b,c" ?,)
                :type 'error))

(ert-deftest test-mb-csv-read-field ()
  "Test reading individual CSV fields and advancing the cursor."
  (let ((line "a,\"b,c\",d"))
    (should (equal (mb-csv--read-field line 0 ?,) '("a" . 2)))
    (should (equal (mb-csv--read-field line 2 ?,) '("b,c" . 8)))
    (should (equal (mb-csv--read-field line 8 ?,) '("d")))))

(ert-deftest test-csv-string ()
  "Test CSV serialization."
  (should (equal (csv-string '((a "b" nil) (1 2 3)))
                 "a;\"b\";\n1;2;3"))
  (should (equal (csv-string '((a nil c)) "," "\r\n" "NULL")
                 "a,NULL,c")))

(ert-deftest test-mb-utils-io-byte-vector-round-trip ()
  "Test binary vector writing, slicing, and reading."
  (let ((file (make-temp-file "mb-utils-io-test-")))
    (unwind-protect
        (progn
          (write-byte-vector [0 1 127 128 255] file :from 1 :to 5)
          (should (equal (read-byte-vector file) [1 127 128 255]))
          (should (equal (read-byte-vector file :from 1 :to 3) [127 128])))
      (delete-file file))))

(provide 'test-mb-utils-io)

;;; test-mb-utils-io.el ends here
