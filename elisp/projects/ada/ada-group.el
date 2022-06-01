(require 'emacsql-mysql)

(defun cancel-group-sync (group-id-descriptor)
  (emacsql db [:update company-group :set (= sync-date $r1) :where (= id $s2)]
	   (iso-date-and-time :time (add-etime-date (now) :day -1))
	   (id group-id-descriptor)))
;;(cancel-group-sync 571156)

(defun company-group (id-descriptor)
  (car (emacsql db [:select * :from company-group :where (= id $s1)] (id id-descriptor))))
;;(company-group (company-group 571179))
;;(company-group 571179)

(defun group-members (company-group-descriptor)
  (mapcar #'car
    (emacsql db [:select user-id
		 :from user-company-group
		 :where (= company-group-id $s1)]
	    (id company-group-descriptor))))
;;(cl-sort (mapcar (compose #'sstring #'fourth #'car #'user) (group-members 571184)) #'string<)
;;(company-group 571156)

(defun delete-group-member (user-id-descriptor)
  (emacsql db [:delete :from user-company-group :where (= user-id $s1)]
	   (id user-id-descriptor)))
;;(delete-group-member (id (user-from-name "%laerer%99_6%")))

(defun delete-members-in-group (group-descriptor)
  (emacsql db [:delete :from user-company-group :where (= company-group-id $s1)]
	   (id group-descriptor)))
;;(id 571184)

;;(defconst teacher9-1 (user-from-name "%laerer%9_1%"))

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
;;(group-ids-from-user 321210)
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
  "Return active company-groups"
  (emacsql db
    [:select *
      :from company-group
      :where (< from-date $r1 to-date)
      ]
    (iso-date-and-time)))
;;(active-groups)

(defun active-group-ids ()
  "Return ID of active company-groups"
  (mapcar #'car
    (emacsql db
      [:select id
	:from company-group
	:where (< from-date $r1 to-date)
	]
      (iso-date-and-time))))
;;(active-group-ids)

(defun active-singleton-company-groups ()
  "Return groups with only one member"
  (copy 1 (mapcar #'company-group-size (active-group-ids))
    :key #'second))
;;(active-company-group-sizes)

(defun group-member-p (user-id company-group-id)
  (emacsql db
    [:select *
      :from user-company-group
      :where (and (= company-group-id $s1)
		  (= user-id $s2))]
    company-group-id user-id))
;;(group-member-p (caar (user-from-name "%laerer%9_1%")) (active-group-ids))

(defun active-singleton-company-groups-user (user-id-descriptor)
  (loop for (company-group-id length) in (active-singleton-company-groups)
	if (group-member-p (id user-id-descriptor) company-group-id)
	collect company-group-id))
;;(active-singleton-company-groups-user teacher9-1)

(provide 'ada-group)
