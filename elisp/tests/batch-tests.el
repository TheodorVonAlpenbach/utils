;;; batch-tests.el --- Batch entry point for the maintained ERT baseline -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-manifest)
(mb-tests-load-baseline)

(ert-run-tests-batch-and-exit t)

;;; batch-tests.el ends here
