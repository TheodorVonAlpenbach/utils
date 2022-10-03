(require 'ada-mysql)

(defun json-from-id (json-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'json
		 :where '(= id $s1))
	 (id json-id-descriptor))))
;;(json-from-id 195026)

(provide 'ada-json)
