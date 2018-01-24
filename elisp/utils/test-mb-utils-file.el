(require 'ert)
(require 'mb-utils-file)

(ert-deftest test-directory-truename ()
  "Test of `directory-truename'"
  (should (equal (directory-truename "a/b") (file-truename "a/b")))
  (should (equal (directory-truename "a/b/") (file-truename "a/b"))))
