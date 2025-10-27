(require 'ada-mysql)
(require 'ada-component-relations)


(cl-defun adaptive-task-from-source-id (source-id &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'adaptive-task
		      :where '(= source-id $r1))
	   source-id))
;;(setf at (adaptive-task-from-source-id "68e64bd33610530ca8786a92"))

(cl-defun adaptive-task-parameter-sets (adaptive-task &rest columns)
  (when (stringp adaptive-task)
    (setf adaptive-task (car (adaptive-task-from-source-id adaptive-task))))
  (emacsql db (vector :select (column-selection columns)
		      :from 'adaptive-task-parameter-set
		      :where '(= adaptive-task-id $s1))
	   (id adaptive-task)))
;;(setf atps (adaptive-task-parameter-sets "68cacd1804c1a9eea46fe90e"))

(defun print-parameter-sets (parameter-sets)
  (concat* (project-sequence parameter-sets '(2 3))
    :in "\n"
    :key #'(lambda (x) (concat* x :in "\t"))))
;;(print-parameter-sets (adaptive-task-parameter-sets "68c7ee23e50759a222f44f0f"))
;;(length (adaptive-task-parameter-sets "68c43528cc7a98823d344bfb"))

(cl-defun adaptive-task-parameter-set-ratings
    (adaptive-task-parameter-sets &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'adaptive-task-parameter-set-rating
		      :where 'adaptive-task-parameter-set-id :in '$v1)
	   (cl-coerce (ids adaptive-task-parameter-sets) 'vector)))
;;(adaptive-task-parameter-set-ratings atps)
;;(mapcar #'id atps)

(cl-defun delete-adaptive-task-parameter-set-ratings
    (adaptive-task-parameter-sets)
  (emacsql db (vector :delete :from 'adaptive-task-parameter-set-rating
		      :where 'adaptive-task-parameter-set-id :in '$v1)
	   (cl-coerce (ids adaptive-task-parameter-sets) 'vector)))
;;(delete-adaptive-task-parameter-set-ratings atps)

;; Most important function
(cl-defun delete-adaptive-task-parameter-sets (adaptive-task)
  (delete-adaptive-task-parameter-set-ratings
   (ids (adaptive-task-parameter-sets at :id)))
  (emacsql db (vector :delete :from 'adaptive-task-parameter-set
		      :where '(= adaptive-task-id $s1))
	   (id adaptive-task)))
;;(delete-adaptive-task-parameter-sets at)

(provide 'ada-adaptive-task)
