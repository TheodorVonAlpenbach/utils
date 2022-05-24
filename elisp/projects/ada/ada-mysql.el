(require 'emacsql-mysql)

;;; To debug an expression, use emacsql-compile, e.g:
;;; (emacsql-compile db [:select * :from component :where (= source-id $r1)] "617bac2c4075f937656d9d36")

(cl-indent '(emacsql-mysql emacsql) 'prog1)

(cl-defun ada-mysql-connect (&optional (db :ada))
  (destructuring-bind (db user password host port)
      (cl-find db (read* ".pwd") :key #'first)
    (emacsql-mysql (downcase (sstring db))
	     :user user :password password :host host :port port)))
;;(ada-mysql-connect)

(defvar db (emacsql-mysql "ada"
	     :user "root" :password "docker77" :host "127.0.0.1" :port 30100))

(defun id (id-descriptor)
  (when id-descriptor
    (if (and (integerp id-descriptor)
	    (plusp id-descriptor))
     id-descriptor
     (if (listp id-descriptor)
       (id (car id-descriptor))
       (error "Argument is not an ID descriptor!")))))
;;(id (list 1))

(defun cancel-group-sync (group-id-descriptor)
  (emacsql db [:update company-group :set (= sync-date $r1) :where (= id $s2)]
	   (iso-date-and-time :time (add-etime-date (now) :day -1))
	   (id group-id-descriptor)))
;;(cancel-group-sync 571156)

(defun user-id-from-pseudonym (user-pseudonym)
  (caar (emacsql db [:select user-id :from user-pseudonym :where (= user-pseudonym $r1)]
		 user-pseudonym)))
;;(user-id-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun user-from-pseudonym (user-pseudonym)
  (emacsql db [:select * :from user :where (= id $s1)]
	   (user-id-from-pseudonym user-pseudonym)))
;;(user-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun user (user-id-descriptor)
  (if (and (stringp user-id-descriptor) (find ?- user-id-descriptor))
    (user-from-pseudonym user-id-descriptor)
    (emacsql db [:select * :from user :where (= id $s1)] (id user-id-descriptor))))
;;(user "336dd2be-94e8-4f95-b184-adf18d58326f")
 
(defun company-group (id)
  (emacsql db [:select * :from company-group :where (= id $s1)] id))
;;(mapcar #'company-group (active-singleton-company-groups-user (caar (user-from-name "%laerer%9_1%"))))
;;(company-group 571179)

(defun company (id-descriptor)
  (emacsql db [:select * :from company :where (= id $s1)] (id id-descriptor)))
;;(company 2585)

(defun companies ()
  (emacsql db [:select * :from company]))
;;(companies)

(defun company-from-name (name)
  (emacsql db [:select * :from company :where (= name $s1)] (id id-descriptor)))
;;(company 1)
 
(defun group-members (company-group-descriptor)
  (mapcar #'car
    (emacsql db [:select user-id
		 :from user-company-group
		 :where (= company-group-id $s1)]
	    (id company-group-descriptor))))
;;(mapcar (compose #'user) (group-members 571156))
;;(company-group 571156)

(defun delete-group-member (user-id-descriptor)
  (emacsql db [:delete :from user-company-group :where (= user-id $s1)]
	   (id user-id-descriptor)))
;;(delete-group-member (id (user-from-name "%laerer%99_6%")))

(defun delete-user (user-id-descriptor)
  (let ((id (id user-id-descriptor)))
    (emacsql db [:delete :from user-company-group :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-ntp-module-codes :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-pseudonym :where (= user-id $s1)] id)
    (emacsql db [:delete :from user :where (= id $s1)] id)))
;;(delete-user (user-from-name "Celev_no456326499_7a_5 CappelenDamm"))
;;(delete-user 321679)
;;(321679 321779 321211)

(defun user-from-name (name)
  (emacsql db
    [:select * :from user :where (like name $r1)]
    name))
;;(caar (user-from-name "%laerer%99_5%"))

(defconst teacher9-1 (user-from-name "%laerer%9_1%"))

(defun ada-group (id)
  (if (listp id)
    (mapcar #'ada-group id)
    (emacsql db
      [:select * :from company-group
	:where (= id $s1)]
      id)))
;;(ada-group 571157)

(defun group-ids-from-user (user)
  (mapcar #'car (emacsql db
		  [:select company-group-id
		    :from user-company-group
		    :where (= user-id $s1)]
		  (id user))))
;;(group-ids-from-user teacher9-1)
;;(mapcar #'company-group-size (group-ids-from-user teacher9-1))

(defun company-group-size (id)
  (cons id (car (emacsql db
		  [:select (funcall count id)
		    :from user-company-group
		    :where (= company-group-id $s1)]
		  id))))
;;(company-group-size 571179)

(defun active-groups ()
  "Return ID of active company-groups"
  (mapcar #'car
    (emacsql db
      [:select id
	:from company-group
	:where (< from-date $r1 to-date)
	]
      (iso-date-and-time))))
;;(active-groups)

(defun active-singleton-company-groups ()
  "Return groups with only one member"
  (copy 1 (mapcar #'company-group-size (active-groups))
    :key #'second))
;;(active-company-group-sizes)

(defun group-member-p (user-id company-group-id)
  (emacsql db
    [:select *
      :from user-company-group
      :where (and (= company-group-id $s1)
		  (= user-id $s2))]
    company-group-id user-id))
;;(group-member-p (caar (user-from-name "%laerer%9_1%")) (active-groups))

(defun active-singleton-company-groups-user (user-id-descriptor)
  (loop for (company-group-id length) in (active-singleton-company-groups)
	if (group-member-p (id user-id-descriptor) company-group-id)
	collect company-group-id))
;;(active-singleton-company-groups-user teacher9-1)

(defun component-from-source-id (source-id)
  (car (emacsql db [:select * :from component :where (= source-id $r1)] source-id)))

(defun component-from-uuid (uuid)
  (car (emacsql db [:select * :from component :where (= uuid $r1)] uuid)))
;;(component-from-uuid "1cbb9df2-669c-3b44-8c76-ac7f9c39e103")

(defun component-from-string-id (string-id)
  (if (= (length string-id) 36)
    (component-from-uuid string-id)
    (component-from-source-id string-id)))
;;(component-from-string-id "1cbb9df2-669c-3b44-8c76-ac7f9c39e103")

(defun component-from-id (id)
  (car (emacsql db [:select * :from component :where (= id $s1)] id)))
;;(component-from-source-id "617bac2c4075f937656d9d36")

(defun component (id-descriptor)
  (typecase id-descriptor
    (string (component-from-string-id id-descriptor))
    (number (component-from-id id-descriptor))
    (list (mapcar #'component-from-id id-descriptor))
    (otherwise (component-from-id (id id-descriptor)))))
;;(component "1cbb9df2-669c-3b44-8c76-ac7f9c39e103")
;;(mapcar (compose #'car #'component) (list 15752 "617bac2c4075f937656d9d36" "1cbb9df2-669c-3b44-8c76-ac7f9c39e103"))

(defun json-from-id (id)
  (car (emacsql db [:select json :from json :where (= id $s1)] id)))
;;(json-from-id 194581)

(defun component-json (id-descriptor)
  (caar (emacsql db [:select json-id :from component :where (= id $s1)] (id id-descriptor))))
;;(component-json 15752)

(provide 'ada-mysql)
