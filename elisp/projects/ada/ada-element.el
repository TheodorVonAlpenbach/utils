(require 'ada-mysql)
(require 'ada-component)

(cl-defun element-from-id (id &rest columns)
  (car (emacsql db (vector :select (column-selection columns)
			   :from 'element
			   :where '(= id $s1)) id)))
;;(element-from-id 39709 :source-id)

(cl-defun element (element-descriptor &rest columns)
  (cl-typecase element-descriptor
    (number (apply #'element-from-id element-descriptor columns))
    (list (if (source-id-version-p element-descriptor)
	    (apply #'element-from-source-id-version element-descriptor columns)
	    (cl-loop for x in element-descriptor
		     collect (apply #'element x columns))))
    (otherwise (apply #'element-from-id (id element-descriptor) columns))))
;;(element 39709 :id)

(cl-defun element-id (element-descriptor)
  (car (ada-parse-id-list (element element-descriptor :id))))
;;(element-id 39709)

(cl-defun component-element-ids (component)
  (ada-parse-id-list
   (emacsql db [:select element-id :from component-element
		 :where (= component-id $s1)]
	    (component-id component))))
;;(component-element-ids "64a6935f4d2d21688abd1a1d")

(provide 'ada-task)
