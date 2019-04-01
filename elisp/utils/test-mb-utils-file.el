(require 'ert)
(require 'mb-utils-file)

(ert-deftest test-parent-directory ()
  "Test of `parent-directory'"
  (should (equal (parent-directory "/a/b/c") "/a/b/"))
  (should (equal (parent-directory "/a/b/c/") "/a/b/"))
  (should (equal (parent-directory (parent-directory "/a/b/c/")) "/a/")))

(ert-deftest test-directory-truename ()
  "Test of `directory-truename'"
  (should (equal (directory-truename "a/b") (file-truename "a/b")))
  (should (equal (directory-truename "a/b/") (file-truename "a/b"))))
