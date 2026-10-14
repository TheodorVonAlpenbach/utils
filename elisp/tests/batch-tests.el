;;; batch-tests.el --- Batch entry point for the maintained ERT baseline -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defconst mb-tests-elisp-directory
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(dolist (directory
         (list mb-tests-elisp-directory
               (expand-file-name "utils" mb-tests-elisp-directory)))
  (add-to-list 'load-path directory))

(defconst mb-tests-baseline-files
  '("test-list-interval.el"
    "test-list-envelope.el"
  "utils/test-mb-utils-io.el"
  "utils/test-mb-utils-sets.el"))

(dolist (file mb-tests-baseline-files)
  (load (expand-file-name file mb-tests-elisp-directory)
        nil nil t))

(ert-run-tests-batch-and-exit t)

;;; batch-tests.el ends here
