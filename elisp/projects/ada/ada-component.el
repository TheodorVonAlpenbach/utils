(require 'ada-mysql)
(require 'ada-json)

(defun components-from-source-id (source-id &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'component
		      :where '(= source-id $r1)) source-id))
;;(car (components-from-source-id "65e0b7be720a665ffafad6db" :id :uuid))("65169" "3b0828c7-5783-377e-9635-3e68c93d747f")
;;(caar (components-from-source-id "5f57406a9abf8c1727301221" :uuid))
;;(components-from-source-id "5f57406a9abf8c1727301221" :id :version :uuid)

(defun latest-component-id-from-source-id (source-id)
  (id (min-element (components-from-source-id source-id :id :version)
		   :test #'> :key (compose #'string-to-integer #'second))))
;;(latest-component-id-from-source-id "6422c4c1520bcb49b4523f64")

(defun latest-component-from-source-id (source-id &rest columns)
  (apply #'component (latest-component-id-from-source-id source-id) columns))
;;(latest-component-from-source-id "6422c4c1520bcb49b4523f64")

(defun component-from-uuid (uuid &rest columns)
  (car (emacsql db (vector :select (column-selection columns)
			   :from 'component
			   :where '(= uuid $r1)) uuid)))
;;(component-from-uuid "c14292e2-b07d-3dba-8699-04e9af44b71b" :source-id)("")

;;(component-from-uuid "ec8a92fd-b300-3d18-93e4-89245e4b46a1" 'internal-title 'source-id)

(defun component-from-string-id (string-id &rest columns)
  (if (= (length string-id) 36)
    (apply #'component-from-uuid string-id columns)
    (apply #'latest-component-from-source-id string-id columns)))
;;(component-from-string-id "6422c4c1520bcb49b4523f64" 'uuid)

(defun component-from-id (id &rest columns)
  (car (emacsql db (vector :select (column-selection columns)
			   :from 'component
			   :where '(= id $s1)) id)))

(defun source-id-version-p (x)
  (and (listp x) (= (length x) 2) (stringp (first x)) (stringp (second x))))
;;(source-id-version-p '("qwe" "123"))

(defun component-from-source-id-version (source-id-version &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'component
		 :where '(and (= source-id $r1) (= version $s2)))
	 (first source-id-version)
	 (string-to-integer (second source-id-version)))))
;;(component-from-source-id-version '("5fa944108fdf333a47c0a94d" "4") :id :uuid)("20192" "")

(defun component (component-descriptor &rest columns)
  (cl-typecase component-descriptor
    (string (apply #'component-from-string-id component-descriptor columns))
    (number (apply #'component-from-id component-descriptor columns))
    (list (if (source-id-version-p component-descriptor)
	    (apply #'component-from-source-id-version
	      component-descriptor columns)
	    (cl-loop for x in component-descriptor
		     collect (apply #'component x columns))))
    (otherwise (apply #'component-from-id (id component-descriptor) columns))))
;;(car (component 16538 :uuid))
;;(cl-loop for id in (list 16538 31962 31963 31964 31967) collect (car (component id :uuid)))

(defun curriculum-ids-from-component-id (component-id)
  (ada-parse-id-list
    (emacsql db
      [:select curriculum-id :from component-curriculum :where (= component-id $s1)]
      component-id)))
;;(curriculum-ids-from-component-id 15955)
 
(defun component-curriculums (component-descriptor)
  (aif (component component-descriptor :id)
    (gateway-ids-from-component-id (string-to-integer (car it)))
    (error "No such component!")))
;;(component-gateways 15955)

(defun component-gateways (component-descriptor)
  (aif (component component-descriptor :id)
    (gateway-ids-from-component-id (string-to-integer (car it)))
    (error "No such component!")))
;;(component-gateways 15955)

(defun ada-parse-id-list (id-list)
  (mapcar #'string-to-integer (flatten id-list)))
;;(ada-parse-id-list '(("1") ("2")))

(defun component-id (component-descriptor)
  (car (ada-parse-id-list (component component-descriptor :id))))
;;(component-id "64a6935f4d2d21688abd1a1d")

(defun component-element-ids (component-descriptor)
  (ada-parse-id-list
   (emacsql db [:select element-id :from component-element
		 :where (= component-id $s1)]
	    (component-id component-descriptor))))
;;(component-element-ids 15955)

(defun sub-competence-aim-ids-from-element-ids (element-ids)
  (ada-parse-id-list
   (emacsql db [:select sub-competence-aim-id :from element-sub-competence-aim
		 :where element-id :in $v1]
	    (coerce element-ids 'vector))))
;;(sub-competence-aim-ids-from-element-ids (component-element-ids 15955))

(defun sub-competence-aim (id &rest columns)
  (car  (emacsql db
	  (vector :select (column-selection columns)
		  :from 'sub-competence-aim
		  :where '(= id $s1))
	  id)))
;;(sub-competence-aim 7034)

(defun competence-aim (id)
  (emacsql db [:select * :from competence-aim :where (= id $s1)] id))
;;(sub-competence-aim (car (ada-parse-id-list (sub-competence-aim 7034 :competence-aim-id))))

(defun curriculum-ids-from-sub-competence-aim-ids (sub-competence-aim-ids)
  (ada-parse-id-list
   (emacsql db [:select curriculum-id :from sub-competence-aim-curriculum
		 :where sub-competence-aim-id :in $v1]
	    (coerce sub-competence-aim-ids 'vector))))
;;(curriculum-ids-from-sub-competence-aim-ids '(7034))

(defun component-sub-competence-aims-from-elements (component-descriptor)
  (mapcar #'sub-competence-aim
    (sub-competence-aim-ids-from-element-ids
     (component-element-ids component-descriptor))))

(defun component-sub-competence-aims (component-descriptor)
  (component-sub-competence-aim-ids component-descriptor))

(defun component-all-sub-competence-aims (component-descriptor)
  (component-sub-competence-aims-from-elements component-descriptor))
;;(component-sub-competence-aims-from-elements 15959)

(defun component-json (component-descriptor)
  (aif (component component-descriptor :json-id)
    (json-from-id (string-to-integer (car it)))
    (error "No such component!")))
;;(component-json '("6333f33ce2789f1fe3071906" "4"))

(defun fsub-competence-aim (sub-competence-aim-descriptor &rest columns)
  "Arugments COLUMNS are not yet supported"
  (tab-format
   (butlast
    (cl-loop for v in (sub-competence-aim sub-competence-aim-descriptor)
		  for (k . rest ) in (ada-columns 'sub-competence-aim)
		  collect (list k v)))))
;;(fsub-competence-aim (string-to-integer (caar (component-sub-competence-aims-from-elements 15959))))

(defun fcomponent (component-descriptor &rest columns)
  "Arugments COLUMNS are not yet supported"
  (tab-format (butlast (cl-loop for v in (component component-descriptor)
				for (k . rest ) in (ada-columns 'component)
				collect (list k v)))))
;;(fcomponent 15742)

;;(component 15761 'internal-title)
;;(car (component 15761 'uuid))
;;(parse-ms-unix-time )

(defun delete-component (component-id-descriptor)
  (let* ((component (component component-id-descriptor))
	 (id (string-to-integer (car component))))
    (emacsql db [:delete :from component-element :where (= component-id $s1)] id)
    (emacsql db [:delete :from component :where (= id $s1)] id)))
;;(delete-component 15984)
;;(component 15984)

(provide 'ada-component)
