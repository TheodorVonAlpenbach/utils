;;; test-manifest.el --- Maintained Emacs Lisp test manifest -*- lexical-binding: t; -*-

(defconst mb-tests-elisp-directory
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(defconst mb-tests-load-directories
  '("" "mode-extensions" "utils"))

(defconst mb-tests-baseline-files
  '("test-list-interval.el"
    "test-list-envelope.el"
    "mode-extensions/test-mb-ert.el"
    "utils/test-mb-anaphora.el"
    "utils/test-mb-math-functions.el"
    "utils/test-mb-lists.el"
    "utils/test-mb-sequences.el"
    "utils/test-mb-things.el"
    "utils/test-mb-utils-div.el"
    "utils/test-mb-utils-economy.el"
    "utils/test-mb-utils-file.el"
    "utils/test-mb-utils-io.el"
    "utils/test-mb-utils-iso-time.el"
    "utils/test-mb-utils-sets.el"
    "utils/test-mb-utils-strings.el"
    "utils/test-mb-utils-time.el"
    "utils/test-mb-utils-tree.el"))

(defun mb-tests-add-load-paths ()
  "Add maintained test directories to `load-path'."
  (dolist (directory mb-tests-load-directories)
    (add-to-list 'load-path
                 (expand-file-name directory mb-tests-elisp-directory))))

(defun mb-tests-load-baseline ()
  "Load every test file in the maintained baseline."
  (mb-tests-add-load-paths)
  (dolist (file mb-tests-baseline-files)
    (load (expand-file-name file mb-tests-elisp-directory)
          nil nil t)))

(provide 'test-manifest)

;;; test-manifest.el ends here
