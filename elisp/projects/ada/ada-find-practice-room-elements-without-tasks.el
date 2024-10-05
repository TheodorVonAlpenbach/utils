(require 'ada-component)

(cl-defun find-practice-room-element-ids ()
  (mapcar #'id (emacsql db
     [:select * :from element :where (like source-type $r1)]
     "%ractice%")))
;;(find-practice-room-element-ids)

(cl-defun task-id-from-element-id (element-id)
  (id (emacsql db
	[:select id :from task :where (= element-id $s1)]
	element-id)))
;;(task-id-from-element-id (car (find-practice-room-element-ids)))

(cl-defun component-id-from-element-id (element-id)
  (id (emacsql db
     [:select component-id :from component-element :where (= element-id $s1)]
     element-id)))
;;(component-id-from-element-id 39715)

(cl-defun find-practice-room-elements-without-tasks ()
  (cl-loop for element-id in (find-practice-room-element-ids)
	   for task-id = (task-id-from-element-id element-id)
	   if (not task-id) collect element-id))
;;(find-practice-room-elements-without-tasks)
;; utv => (39691 39692 39693 39694 39695 39696 39777)

(cl-defun find-practice-room-components-without-tasks ()
  (cl-loop for element-id in (find-practice-room-elements-without-tasks)
	   for component-id = (component-id-from-element-id element-id)
	   for x = (component component-id :source-id :internal-title)
	   collect x))

(cl-defun find-practice-room-components-without-tasks ()
  (cl-loop for component-id
	   in (remove-duplicates
		  (mapcar #'component-id-from-element-id
		    (find-practice-room-elements-without-tasks)))
	   collect (component component-id :id :source-id :internal-title)))
;;(find-practice-room-components-without-tasks)
;;utv => (("626654d8ec75943f081c7513" "Eivinds øverom") ("62f3c49410db883d10c76e06" "ADA-10390 (Peter, 1)"))
;; (setf tab '(("38487" "6273d57ed6bc8153137604c6" "Øverom: Norsk 2-4 - Tema 3 - Tivoliet - Diftonger") ("38499" "6273d52dd6bc8143317604bc" "Øverom: Norsk 2-4 - Tema 1 - På rømmen - Grafemer - Ord med -ng, -nk og -gn") ("38500" "6273d5246322078db9527541" "Øverom: Norsk 2-4 - Tema 1 - På rømmen - Grafemer - Diftonger") ("39110" "6273d5246322078db9527541" "Øverom: Norsk 2-4 - Tema 1 - På rømmen - Grafemer - Diftonger") ("39120" "6273d5246322078db9527541" "Øverom: Norsk 2-4 - Tema 1 - På rømmen - Grafemer - Diftonger") ("39123" "6273d52dd6bc8143317604bc" "Øverom: Norsk 2-4 - Tema 1 - På rømmen - Grafemer - Ord med -ng, -nk og -gn") ("41330" "62fe33807da0dd48fc08e6a0" "Wenche tester øverom 18.08.22") ("41331" "62fe36992b36972ac00216fe" "Wenche tester øverom 2 - 18.08.22")))
;;(tab-format tab :header '("mysql-id" "source-id" "intern tittel") :column-separator "  ")

(cl-defun practice-room-components-without-tasks-report ()
  (concat* '(("626654d8ec75943f081c7513" "Eivinds øverom") ("62f3c49410db883d10c76e06" "ADA-10390 (Peter, 1)"))
    :in "\n"
    :key #'(lambda (x) (concat* (sstring x) :in "\t"))))
;;(practice-room-components-without-tasks-report)
;;utv => (("626654d8ec75943f081c7513" "Eivinds øverom") ("62f3c49410db883d10c76e06" "ADA-10390 (Peter, 1)"))

(provide 'ada-find-practice-room-elements-without-tasks)
