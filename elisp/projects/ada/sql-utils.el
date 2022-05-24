(require 'mb-utils-strings)

(defun sql-list (list)
  (concat* list :pre "(" :in "," :suf ")" :key #'sstring))
;;(sql-list '(1 2 3))

(provide 'sql-utils)
