(require 'ada-mysql)

(defun task-from-id (task-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'task
		 :where '(= id $s1))
	 (id task-id-descriptor))))
;;(car (task-from-id 50014 'solution-id))

(defun task-solution (task-id-descriptor)
  (car (task-from-id 50014 'solution-id)))
;;(json-from-id (task-solution 50014) 'json)
