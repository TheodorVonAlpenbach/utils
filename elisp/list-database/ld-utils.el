;;; ld-utils.el --- Small list-database utilities -*- lexical-binding: t; -*-

;;; Code:

(require 'ld-column)

(defun ld-check-type (value column-definition)
  "Return non-nil when VALUE has COLUMN-DEFINITION's declared type."
  (let ((type (ld-column-type column-definition)))
    (or (null type)
        (eq (type-of value) type))))
;;(ld-check-type "qwe" '(:qwe "" nil))

(defun keyword->filename (keyword)
  "Convert KEYWORD to a lowercase filename component."
  (unless (keywordp keyword)
    (user-error "Not a keyword: %S" keyword))
  (downcase (substring (symbol-name keyword) 1)))

(provide 'ld-utils)

;;; ld-utils.el ends here
