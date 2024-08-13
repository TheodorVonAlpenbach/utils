(require 'ada-mysql)

(defun json-from-id (json-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'json
		 :where '(= id $s1))
	 (id json-id-descriptor))))
;;(setf qwe (json-from-id 193551))

(provide 'ada-json)
