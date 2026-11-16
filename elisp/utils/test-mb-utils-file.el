;;; test-mb-utils-file.el --- Tests for mb-utils-file -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-file)

(defmacro mb-utils-file-test--with-temp-directory (directory &rest body)
  "Create DIRECTORY, evaluate BODY, and remove the directory."
  (declare (indent 1) (debug t))
  `(let ((,directory (make-temp-file "mb-utils-file-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,directory t))))

(ert-deftest mb-utils-file-parent-directory ()
  "Test of `parent-directory'"
  (should (equal (parent-directory "/a/b/c") "/a/b/"))
  (should (equal (parent-directory "/a/b/c/") "/a/b/"))
  (should (equal (parent-directory "/a/b/c/" 2) "/a/"))
  (should (equal (parent-directory "/a/b/c/" 0) "/a/b/c/"))
  (should (equal (parent-directory "/" 3) "/"))
  (should-error (parent-directory "/a/b/c" -1)))

(ert-deftest mb-utils-file-expand-directory-name ()
  "Test of `expand-directory-name'"
  (mb-utils-file-test--with-temp-directory parent
    (let ((child (expand-file-name "child" parent)))
      (make-directory child)
      (should (equal (expand-directory-name ".." child)
                     (file-name-as-directory parent)))))
  (should (equal (expand-directory-name "..")
		 (expand-directory-name ".." default-directory))))

(ert-deftest mb-utils-file-directory-truename ()
  "Test of `directory-truename'"
  (mb-utils-file-test--with-temp-directory directory
    (let ((child (expand-file-name "a/b" directory)))
      (make-directory child t)
      (should (equal (directory-truename child)
                     (file-truename child)))
      (should (equal (directory-truename (file-name-as-directory child))
                     (file-truename child))))))

(ert-deftest mb-utils-file-in-directory-p ()
  "Test directory membership without sibling-prefix false positives."
  (mb-utils-file-test--with-temp-directory root
    (let ((directory (expand-file-name "project" root))
          (sibling (expand-file-name "project-backup/file" root)))
      (make-directory directory)
      (should (in-directory-p (expand-file-name "file" directory) directory))
      (should-not (in-directory-p sibling directory)))))

(ert-deftest mb-utils-file-find-filename ()
  "Test readable file lookup and directory priority."
  (mb-utils-file-test--with-temp-directory root
    (let ((first-directory (expand-file-name "first" root))
          (second-directory (expand-file-name "second" root)))
      (make-directory first-directory)
      (make-directory second-directory)
      (with-temp-file (expand-file-name "target" second-directory))
      (should (equal (find-filename
                      "target" (list first-directory second-directory))
                     (expand-file-name "target" second-directory)))
      (with-temp-file (expand-file-name "target" first-directory))
      (should (equal (find-filename
                      "target" (list first-directory second-directory))
                     (expand-file-name "target" first-directory)))
      (should
       (equal (find-filenames
               '("target" "missing")
               (list first-directory second-directory))
              (list (expand-file-name "target" first-directory) nil))))))

(ert-deftest mb-utils-file-directory-files ()
  "Test concatenating directory listings."
  (mb-utils-file-test--with-temp-directory root
    (let ((first (expand-file-name "first" root))
          (second (expand-file-name "second" root)))
      (make-directory first)
      (make-directory second)
      (with-temp-file (expand-file-name "a.txt" first))
      (with-temp-file (expand-file-name "b.txt" second))
      (should
       (equal (directory-files*
               (list first second) t "\\.txt\\'")
              (list (expand-file-name "a.txt" first)
                    (expand-file-name "b.txt" second)))))))

(ert-deftest mb-utils-file-name-transformations ()
  "Test filename and extension transformations."
  (should (equal (filename-base "/tmp/archive.tar.gz") "archive.tar"))
  (should (equal (file-name-alter "/tmp/file.txt" "renamed")
                 "/tmp/renamed.txt"))
  (should (equal (file-name-append "/tmp/file.txt" "-old")
                 "/tmp/file-old.txt"))
  (should (equal (file-name-append "/tmp/file.txt" "-old" nil)
                 "/tmp/file-old"))
  (should (equal (file-name-change-extension "/tmp/file.txt" "org")
                 "/tmp/file.org")))

(ert-deftest mb-utils-file-rename-files ()
  "Test literal matching while renaming regular files."
  (mb-utils-file-test--with-temp-directory directory
    (with-temp-file (expand-file-name "part[1].txt" directory))
    (with-temp-file (expand-file-name "part1.txt" directory))
    (make-directory (expand-file-name "directory[1]" directory))
    (rename-files directory "[1]" "2")
    (should (file-exists-p (expand-file-name "part2.txt" directory)))
    (should (file-exists-p (expand-file-name "part1.txt" directory)))
    (should (file-directory-p (expand-file-name "directory[1]" directory)))
    (should-error (rename-files directory "" "ignored"))))

(provide 'test-mb-utils-file)

;;; test-mb-utils-file.el ends here
