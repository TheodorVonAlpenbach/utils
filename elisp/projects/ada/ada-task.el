(require 'ada-mysql)

(cl-defun task-from-id (task-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'task
		 :where '(= id $s1))
	 (id task-id-descriptor))))
;;(task-from-id 50014)
;;(car (task-from-id 50014 'solution-id))

(cl-defun task (task &rest columns)
  (apply #'task-from-id task columns))
;;()

(require 'ada-element)
(cl-defun element-tasks (element &rest columns)
  (emacsql db
	 (vector :select (column-selection columns)
		 :from 'task
		 :where '(= element-id $s1))
	 (element-id element)))
;;(element-tasks 39709)
;;(element-tasks 39671)

(cl-defun component-tasks (component &rest columns)
  (cl-loop for x in (component-element-ids component) 
	append (apply #'element-tasks x columns)))
;;(component-tasks "64a6935f4d2d21688abd1a1d" :solution-id)
;;(id (component-tasks "64a6935f4d2d21688abd1a1d" :solution-id))

(cl-defun task-element (task)
  (id (task task)))
;;(task-element 50085)

;;(car (json-from-id (id (component-tasks "64a6935f4d2d21688abd1a1d" :solution-id)) 'json))
;;(car (json-from-id (id (element-tasks 39671 :solution-id)) 'json))

(provide 'ada-task)
