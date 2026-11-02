;;; mb-paragraph.el --- Compatibility loader for paragraph helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; The canonical implementation moved to ../elisp/mb-paragraph.el.  Keep this
;; file so configurations that put global/ first on `load-path' continue to
;; resolve (require 'mb-paragraph) correctly.

;;; Code:

(unless (featurep 'mb-paragraph)
  (load (expand-file-name
         "../elisp/mb-paragraph"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil nil))

(provide 'mb-paragraph)

;;; mb-paragraph.el ends here
