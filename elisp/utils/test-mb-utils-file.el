;;; test-mb-utils-file.el --- Tests for mb-utils-file -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-file)

(defmacro test-mb-utils-file-with-temp-directory (directory &rest body)
  "Create DIRECTORY, evaluate BODY, and remove the directory."
  (declare (indent 1) (debug t))
  `(let ((,directory (make-temp-file "mb-utils-file-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,directory t))))

(ert-deftest test-parent-directory ()
  "Test of `parent-directory'"
  (should (equal (parent-directory "/a/b/c") "/a/b/"))
  (should (equal (parent-directory "/a/b/c/") "/a/b/"))
  (should (equal (parent-directory (parent-directory "/a/b/c/")) "/a/")))

(ert-deftest test-expand-directory-name ()
  "Test of `expand-directory-name'"
  (test-mb-utils-file-with-temp-directory parent
    (let ((child (expand-file-name "child" parent)))
      (make-directory child)
      (should (equal (expand-directory-name ".." child)
                     (file-name-as-directory parent)))))
  (should (equal (expand-directory-name "..")
		 (expand-directory-name ".." default-directory))))

(ert-deftest test-directory-truename ()
  "Test of `directory-truename'"
  (test-mb-utils-file-with-temp-directory directory
    (let ((child (expand-file-name "a/b" directory)))
      (make-directory child t)
      (should (equal (directory-truename child)
                     (file-truename child)))
      (should (equal (directory-truename (file-name-as-directory child))
                     (file-truename child))))))

(ert-deftest test-in-directory-p ()
  "Test directory membership without sibling-prefix false positives."
  (test-mb-utils-file-with-temp-directory root
    (let ((directory (expand-file-name "project" root))
          (sibling (expand-file-name "project-backup/file" root)))
      (make-directory directory)
      (should (in-directory-p (expand-file-name "file" directory) directory))
      (should-not (in-directory-p sibling directory)))))

(ert-deftest test-find-filename ()
  "Test readable file lookup and directory priority."
  (test-mb-utils-file-with-temp-directory root
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
                     (expand-file-name "target" first-directory))))))

(ert-deftest test-file-name-transformations ()
  "Test filename and extension transformations."
  (should (equal (filename-base "/tmp/archive.tar.gz") "archive.tar"))
  (should (equal (file-name-append "/tmp/file.txt" "-old")
                 "/tmp/file-old.txt"))
  (should (equal (file-name-change-extension "/tmp/file.txt" "org")
                 "/tmp/file.org")))

(ert-deftest test-copy-libs ()
  "Test recursive copying of Scilab source files."
  (test-mb-utils-file-with-temp-directory root
    (let ((source (expand-file-name "source" root))
          (target (expand-file-name "target" root)))
      (make-directory (expand-file-name "nested" source) t)
      (with-temp-file (expand-file-name "main.sci" source)
        (insert "main"))
      (with-temp-file (expand-file-name "nested/helper.sce" source)
        (insert "helper"))
      (with-temp-file (expand-file-name "ignored.txt" source)
        (insert "ignored"))
      (copy-libs source target)
      (should (file-exists-p (expand-file-name "main.sci" target)))
      (should (file-exists-p (expand-file-name "nested/helper.sce" target)))
      (should-not (file-exists-p (expand-file-name "ignored.txt" target))))))

(provide 'test-mb-utils-file)

;;; test-mb-utils-file.el ends here
