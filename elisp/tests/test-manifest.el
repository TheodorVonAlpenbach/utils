;;; test-manifest.el --- Maintained Emacs Lisp test manifest -*- lexical-binding: t; -*-

(defconst mb-tests-elisp-directory
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(defconst mb-tests-load-directories
  '("" "mode-extensions" "prototypes/geometry" "utils"))

(defconst mb-tests-baseline-files
  '("prototypes/geometry/test-list-interval.el"
    "prototypes/geometry/test-list-envelope.el"
    "mode-extensions/test-mb-ert.el"
    "mode-extensions/test-mb-js-mode.el"
    "mode-extensions/test-mb-org.el"
    "mode-extensions/test-mb-python.el"
    "mode-extensions/test-mb-ruby.el"
    "mode-extensions/test-mb-texinfo.el"
    "utils/test-mb-anaphora.el"
    "utils/test-mb-combinatorics.el"
    "utils/test-mb-polynomials.el"
    "utils/test-mb-table.el"
    "utils/test-mb-math-functions.el"
    "utils/test-mb-utils-math.el"
    "utils/test-mb-lists.el"
    "utils/test-mb-sequences.el"
    "utils/test-mb-things.el"
    "utils/test-mb-utils-div.el"
    "utils/test-mb-utils-economy.el"
    "utils/test-mb-utils-buffer.el"
    "utils/test-mb-utils-file.el"
    "utils/test-mb-utils-geometry.el"
    "utils/test-mb-utils-io.el"
    "utils/test-mb-utils-iso-time.el"
    "utils/test-mb-utils-matrix.el"
    "utils/test-mb-utils-physics.el"
    "utils/test-mb-utils-regexp.el"
    "utils/test-mb-utils-search.el"
    "utils/test-mb-utils-sets.el"
    "utils/test-mb-utils-strings.el"
    "utils/test-mb-utils-time.el"
    "utils/test-mb-utils-tree.el"
    "utils/test-mb-utils-xml.el"))

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
