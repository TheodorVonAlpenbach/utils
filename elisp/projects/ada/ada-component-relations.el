;; See
;; /home/mats/ada/node/admin-api/src/utils/componentRelationsUtils.ts
;; for a definition
(require 'ada-mysql)

(defun gateway-conditions (gateway-id &rest columns)
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'gateway-condition
		 :where '(= gateway-id $s1))
    gateway-id))
;;(gateway-conditions 4 :id)
;;(mapcar #'car (ada-columns 'gateway-condition))
;;(component 40938)

(defun ids (x) (mapcar #'id x))
;;(ids '(("17" "17")))

(defun gateway-content-list-ids (gateway-id)
  (ids
   (car
    (emacsql db
      [:select [service-menu-icon-list-id service-menu-student-icon-list-id]
	:from gateway
	:where (= id $s1)]
      gateway-id))))
;;(gateway-content-list-ids 4)

(defun gateway-content-list-json-ids (gateway-id)
  (ids (emacsql db
	 [:select json-id :from content-list
	   :where id :in $v1]
	 (coerce (gateway-content-list-ids gateway-id) 'vector))))
;;(gateway-content-list-json-ids 4)
;;(json-available-p)

(defun gateway-content-list-jsons (gateway-id)
  (emacsql db
    [:select json :from json
      :where id :in $v1]
    (coerce (gateway-content-list-json-ids gateway-id) 'vector)))
;;(json-parse-string (caar (gateway-content-list-jsons 4)))

(defun ada-get-symbolic-folder-items (content-list-json)
  "Return vector of symbolic folder items as lisp json
objects (hash tables)"
  (copy "symbolicFolder" (gethash "items" (json-parse-string content-list-json))
	:test #'string=
	:key (bind #'gethash "itemType" 1)))
;;(ada-get-symbolic-folder-items (caar (gateway-content-list-jsons 4)))

(defun ada-get-symbolic-folders (content-list-json)
  (cl-loop for x across (ada-get-symbolic-folder-items content-list-json)
	   if (gethash "symbolicFolder" x)
	   collect it))
;;(ada-get-symbolic-folders (caar (gateway-content-list-jsons 4)))

(require 'ada-component)
(defun ada-symbolic-folder-ids-in-content-lists (gateway-id)
  (mapcar #'latest-component-id-from-source-id
    (cl-loop for x in (gateway-content-list-jsons gateway-id)
	     append (ada-get-symbolic-folders (car x)))))
;;(ada-symbolic-folder-ids-in-content-lists 4)

(defun gateway-article-folder-ids (gateway-id)
  (ids (emacsql db
	 [:select :distinct article-id
	   :from gateway-article
	   :where (= gateway-id $s1)]
	 gateway-id)))
;;(gateway-article-folder-ids 4)

(defun gateway-syllabus-folder-ids (gateway-id)
  (ids (emacsql db
	 [:select :distinct folder-id
	   :from gateway-syllabus
	   :where (= gateway-id $s1)]
	 gateway-id)))
;;(gateway-syllabus-folder-ids 4)

(defun gateway-condition-folder (gateway-condition-id)
  (emacsql db
    [:select *
      :from component
      :where (= id $s1)]
    gateway-condition-id))
;;(gateway-condition-folder 40980)

(defun gateway-condition-folder-ids (gateway-condition-ids)
  (ids (emacsql db
	 [:select :distinct folder-id
	   :from gateway-condition
	   :where id :in $v1]
	 (coerce gateway-condition-ids 'vector))))
;;(gateway-condition-folder-ids (ids (gateway-conditions 4 :id)))

(defun licensed-module-condition-folder-ids (gateway-condition-ids)
  (ids (emacsql db
	 [:select :distinct folder-id
	   :from gateway-licensed-module-condition
	   :where gateway-condition-id :in $v1]
	 (coerce gateway-condition-ids 'vector))))
;;(licensed-module-condition-folder-ids (ids (gateway-conditions 4 :id)))
;;(seq-intersection asd (licensed-module-condition-folder-ids (ids (gateway-conditions 4 :id))))

(defun gateway-application-ids (gateway-condition-ids)
  (ids (emacsql db
	 [:select :distinct application-id
	   :from gateway-condition-application
	   :where gateway-condition-id :in $v1]
	 (coerce gateway-condition-ids 'vector))))
;;(gateway-application-ids (ids (gateway-conditions 4 :id)))

(defun gateway-root-ids (gateway-id)
  (let ((condition-ids (ids (gateway-conditions gateway-id :id))))
    (append (gateway-condition-folder-ids condition-ids)
	    (gateway-application-ids condition-ids)
	    (licensed-module-condition-folder-ids condition-ids)
	    (gateway-syllabus-folder-ids gateway-id)
	    (gateway-article-folder-ids gateway-id)
	    (ada-symbolic-folder-ids-in-content-lists gateway-id))))
;;(seq-intersection (car (last ewq)) (gateway-root-ids 4))
;;(ada-columns 'gateway-condition)
;;(length (component 3901))

(defun target-components-1 (source-ids)
  (when source-ids
    (mapcar (compose #'string-to-integer #'car)
      (emacsql db
	[:select :distinct target-component-id
	  :from component-relations
	  :where source-component-id :in $v1]
	(coerce source-ids 'vector)))))
;;(length (target-components-1 (gateway-conditions 4)))
;;(ada-columns 'component-relations)

(defun target-components (source-ids)
  (cl-loop with all-ids = source-ids
	   for ids = (target-components-1 all-ids) then (target-components-1 new-ids) 
	   for new-ids = (set-difference ids all-ids)
	   while new-ids
	   do (push-list new-ids all-ids)
	   finally return all-ids))
;;(length (target-components (gateway-root-ids 4)))

(defun calculate-gateway-components (gateway-id)
  (target-components (gateway-root-ids gateway-id)))
;;(length (calculate-gateway-components 4))
;;(setf qwe (calculate-gateway-components 4))
;;(find 37021 qwe)

(defun gateway-components (gateway-id)
  (mapcar (compose #'string-to-integer #'car)
    (emacsql db
      [:select component-id :from gateway-components
	:where (= gateway-id $s1)]
      gateway-id)))
;;(length (gateway-components 4))
;;(equal (sort (gateway-components 4) #'<) (sort (calculate-gateway-components 4) #'<))

(defun component-gateways (component-id)
  (mapcar (compose #'string-to-integer #'car)
    (emacsql db
      [:select * :from gateway-components
	:where (= component-id $s1)]
      component-id)))
;;(component-gateways 20192)

(defun all-component-ids ()
  (emacsql db [:select id :from component]))
;;(length (all-component-ids))

(defun source-components-1 (target-ids)
  (when target-ids
    (mapcar (compose #'string-to-integer #'car)
      (emacsql db
	[:select :distinct source-component-id
	  :from component-relations
	  :where target-component-id :in $v1]
	(coerce target-ids 'vector)))))

(defun component-relation (source-id target-id)
  (emacsql db
    [:select * :from component-relations
      :where (= source-component-id $s1)
      :and (= target-component-id $s2)
      ]
    source-id target-id))
;;(component-relation 23933 37022)
;;(loop for (sr tr) in '((8936 17592) (17592 20150) (32293 20150) (40980 40938) (20150 23933) (40938 23933) (23933 37022) (37022 37021)) collect (fourth (car (component-relation sr tr))))
;;(fcomponent 37022)
;;(ada-columns 'component-relations)
;;(fcomponent 40938)
;;(fcomponent 40980)
;;(fcomponent 23933)
;;(ada-columns 'component)

(defun parent-relations (target-ids)
  "Return source-target relations for TARGET-IDS.
For each target-id in TARGET-IDS, if a correponding source-id exists,
return the pair (target-id source-id)."
  (when target-ids
    (maptree #'string-to-integer
      (emacsql db
	[:select :distinct [source-component-id target-component-id]
	  :from component-relations
	  :where target-component-id :in $v1]
	(coerce target-ids 'vector)))))
;;(parent-relations (list 20192))

(defun find-component-ancestors (component-id)
  (cl-loop with all-ids = (list component-id)
	   with all-rels = ()
	   for rels = (parent-relations all-ids) then (parent-relations new-ids) 
	   for ids = (mapcar #'car rels)
	   for new-ids = (set-difference ids all-ids)
	   while new-ids
	   do (push-list new-ids all-ids)
	   do (push-list rels all-rels)
	   finally return (list all-rels all-ids)))
;;(setf ewq-20192 (find-component-ancestors 20192))
;;(setf ewq-16897 (find-component-ancestors 16897))
;;(setf ewq-18161 (find-component-ancestors 18161))
;;(target-components (list 46150))
;;(find-component-ancestors (nth 1 (target-components (list 46150))))
;;(loop for x in (target-components (list 46150)) collect (find-component-ancestors x))

;;((8936 17592) (17592 20150) (32293 20150) (40980 40938) (20150 23933) (40938 23933) (23933 37022) (37022 37021))
;;(8936 -> 17592 -> 20150 -> 23933 -> 37022 -> 37021)
;;(        32293 /         /                         )
;;(        40980 -> 40938 / )
;;(setf asd (list 8936 32293 40980))


(provide 'ada-component-relations)
