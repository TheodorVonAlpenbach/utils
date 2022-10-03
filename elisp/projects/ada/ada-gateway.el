(require 'ada-mysql)

(defun ada-gateway-p (x)
  (and (listp x)
       (> (length x) 10)
       (cl-notany #'listp x)))
;;(ada-gateway-p (gateway-from-id 321211))

(defun gateway-from-name (name &rest columns)
  "Return a list of gateways matching NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(like name $r1))
    name))
;;(gateway-from-name "%laerer%99_5%" :id)

(defun gateway-from-gateway-name (gateway-name &rest columns)
  "Return a list of gateways matching GATEWAY-NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(like gateway-name $r1))
    gateway-name))
;;(caar (gateway-from-name "%laerer%99_5%"))

(defun gateway-id-from-pseudonym (gateway-pseudonym)
  (caar (emacsql db
	  [:select gateway-id
	    :from gateway-pseudonym
	    :where (= gateway-pseudonym $r1)]
	  gateway-pseudonym)))
;;(gateway-id-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun gateway-from-pseudonym (gateway-pseudonym &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(= id $s1))
	 (gateway-id-from-pseudonym gateway-pseudonym))))
;;(gateway-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun gateway-from-id (gateway-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(= id $s1))
	 (id gateway-id-descriptor))))
;;(gateway-from-id 4)
;;(gateway-from-id 4 :id :issuer-id)

(defun gateway-ids-from-component-id (component-id)
  (mapcar (compose #'string-to-integer #'car)
    (emacsql db
      [:select gateway-id :from gateway-components :where (= component-id $s1)]
      component-id)))
;;(gateway-ids-from-component-id 15955)
 
(defun gateway (gateway-descriptor &rest columns)
  (if (stringp gateway-descriptor)
    (apply #'gateway-from-pseudonym gateway-descriptor columns)
    (if (ada-gateway-p gateway-descriptor)
      gateway-descriptor
      (apply #'gateway-from-id gateway-descriptor columns))))
;;(gateway (gateway "eac62d04-2488-4435-b121-87d90c4db9dc"))
;;(gateway-from-id 321211)
;;(gateway 321211)
;;(gateway '(321211))
;;(project (gateway-from-name "%claerer_no456326499_1%") '(0 1 2 3 ))
;;(mapcar #'gateway (list "claerer_no456326499_5%" "336dd2be-94e8-4f95-b184-adf18d58326f"))
 
;;; UPDATE
(defun update-gateway-name (gateway-id-descriptor gateway-name)
  (emacsql db
    [:update gateway :set (= gateway-name $r1) :where (= id $s2)]
    gateway-name gateway-id-descriptor))
;;(update-gateway-name 322168 "claerer_no456326500_4@feide.no")

(provide 'ada-gateway)
