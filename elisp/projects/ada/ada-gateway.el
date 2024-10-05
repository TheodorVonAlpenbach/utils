(require 'ada-mysql)

(cl-defun ada-gateway-p (x)
  (and (listp x)
       (> (length x) 10)
       (cl-notany #'listp x)))
;;(ada-gateway-p (gateway-from-id 321211))

(cl-defun gateway-from-name (name &rest columns)
  "Return a list of gateways matching NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(like name $r1))
    name))
;;(gateway-from-name "%laerer%99_5%" :id)

(cl-defun gateway-from-gateway-name (gateway-name &rest columns)
  "Return a list of gateways matching GATEWAY-NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(like gateway-name $r1))
    gateway-name))
;;(caar (gateway-from-name "%laerer%99_5%"))

(cl-defun gateway-id-from-pseudonym (gateway-pseudonym)
  (caar (emacsql db
	  [:select gateway-id
	    :from gateway-pseudonym
	    :where (= gateway-pseudonym $r1)]
	  gateway-pseudonym)))
;;(gateway-id-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(cl-defun gateway-from-pseudonym (gateway-pseudonym &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(= id $s1))
	 (gateway-id-from-pseudonym gateway-pseudonym))))
;;(gateway-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(cl-defun gateway-from-id (gateway-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'gateway
		 :where '(= id $s1))
	 (id gateway-id-descriptor))))
;;(gateway-from-id 4)
;;(gateway-from-id 4 :id :issuer-id)

(cl-defun gateway-ids-from-component-id (component-id)
  (mapcar (compose #'string-to-integer #'car)
    (emacsql db
      [:select gateway-id :from gateway-components :where (= component-id $s1)]
      component-id)))
;;(gateway-ids-from-component-id 15955)
 
(cl-defun gateway (gateway-descriptor &rest columns)
  (if (stringp gateway-descriptor)
    (apply #'gateway-from-pseudonym gateway-descriptor columns)
    (if (ada-gateway-p gateway-descriptor)
      gateway-descriptor
      (apply #'gateway-from-id gateway-descriptor columns))))
;;(gateway 4)

(cl-defun gateways (&rest columns)
  (emacsql db
    (vector :select (column-selection columns)
	    :from 'gateway)))
;;(cl-delete "NULL" (mapcar #'car (gateways :service-menu-link-list-id)) :test #'string=)

(cl-defun fgateway (gateway-descriptor &rest columns)
  "Arugments COLUMNS are not yet supported"
  (tab-format (butlast (cl-loop for v in (gateway gateway-descriptor)
			     for (k . rest ) in (ada-columns 'gateway)
			     collect (list k v)))))
;;(fgateway 4)

;;; UPDATE
(cl-defun update-gateway-name (gateway-id-descriptor gateway-name)
  (unless (string= (emacsql-psql-dbname db) "ada_prod")
    (emacsql db
      [:update gateway :set (= gateway-name $r1) :where (= id $s2)]
      gateway-name gateway-id-descriptor)))
;;(update-gateway-name 322168 "claerer_no456326500_4@feide.no")

(provide 'ada-gateway)
