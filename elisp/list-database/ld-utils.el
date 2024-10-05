;;; check inserted data
(cl-defun ld-check-type (x column-definition)
  (aif (ld-column-type column-definition)
    (eq (type-of x) it)
    t))
;;(ld-check-type "qwe" '(:qwe "" nil))

(cl-defun keyword->filename (keyword)
  "Standard conversion from KEYWORD to a string that can be part of a path"
  (downcase (keyword-name keyword)))

(provide 'ld-utils)
