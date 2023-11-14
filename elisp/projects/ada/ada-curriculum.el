(require 'ada-mysql)

(defun ada-curriculum-p (x)
  (and (listp x)
       (> (length x) 10)
       (cl-notany #'listp x)))
;;(ada-curriculum-p (curriculum-from-id 321211))

(defun curriculum-from-name (name &rest columns)
  "Return a list of curriculums matching NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'curriculum
		 :where '(like name $r1))
    name))
;;(curriculum-from-name "%laerer%99_5%" :id)

(defun curriculum-from-curriculum-name (curriculum-name &rest columns)
  "Return a list of curriculums matching CURRICULUM-NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'curriculum
		 :where '(like curriculum-name $r1))
    curriculum-name))
;;(caar (curriculum-from-curriculum-name "%laerer%99_5%"))

(defun curriculum-id-from-pseudonym (curriculum-pseudonym)
  (caar (emacsql db
	  [:select curriculum-id
	    :from curriculum-pseudonym
	    :where (= curriculum-pseudonym $r1)]
	  curriculum-pseudonym)))
;;(curriculum-id-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun curriculum-from-pseudonym (curriculum-pseudonym &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'curriculum
		 :where '(= id $s1))
	 (curriculum-id-from-pseudonym curriculum-pseudonym))))
;;(curriculum-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun curriculum-from-id (curriculum-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'curriculum
		 :where '(= id $s1))
	 (id curriculum-id-descriptor))))
;;(curriculum-from-id 3)
;;(curriculum-from-id 4 :id :issuer-id)

(defun curriculum-ids-from-component-id (component-id)
  (mapcar (compose #'string-to-integer #'car)
    (emacsql db
      [:select curriculum-id :from curriculum-component :where (= component-id $s1)]
      component-id)))
;;(curriculum-ids-from-component-id 15955)
 
(defun curriculum (curriculum-descriptor &rest columns)
  (if (stringp curriculum-descriptor)
    (apply #'curriculum-from-pseudonym curriculum-descriptor columns)
    (if (ada-curriculum-p curriculum-descriptor)
      curriculum-descriptor
      (apply #'curriculum-from-id curriculum-descriptor columns))))
;;(curriculum 3)

(defun fcurriculum (curriculum-descriptor &rest columns)
  "Arugments COLUMNS are not yet supported"
  (tab-format (butlast (loop for v in (curriculum curriculum-descriptor)
			     for (k . rest ) in (ada-columns 'curriculum)
			     collect (list k v)))))
;;(fcurriculum 3)

(provide 'ada-curriculum)

0771 141516
