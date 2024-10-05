(require 'emacsql-mysql)
(require 'ada-user)

(cl-defun cancel-group-sync (group-id-descriptor)
  (emacsql db [:update company-group :set (= sync-date $r1) :where (= id $s2)]
	   (iso-date-and-time :time (add-etime-date (now) :day -1))
	   (id group-id-descriptor)))
;;(cancel-group-sync 571156)

(cl-defun company-group (id-descriptor)
  (car (emacsql db [:select * :from company-group :where (= id $s1)] (id id-descriptor))))
;;(company-group 571156)
;;(ada-columns 'company-group)

(cl-defun group-from-name (group-name &optional (company-id 4))
  "Return all groups matching GROUP-NAME and COMPANY-ID.
By default, COMPANY-ID is 4, i.e. Skolen"
  (emacsql db [:select * :from company-group
		:where (like group-name $r1)
		:and (= company-id $s2)
		:and (is deleted-date nil)]
	   group-name company-id))
;;(group-from-name "Basisgruppe 1A")
;;(length (group-from-name "Basisgruppe 1A"))

(cl-defun user-groups (user-descriptor)
  "Return IDs for all groups that includes USER-DESCRIPTOR"
  (mapcar #'id
    (emacsql db [:select company-group-id
		 :from user-company-group
		 :where (= user-id $s1)]
	    (id user-descriptor))))
;;(user-groups 321210)

(cl-defun group-member-ids (group-descriptor)
  "Return user IDs for all members in GROUP-DESCRIPTOR"
  (mapcar #'id
    (emacsql db [:select user-id
		 :from user-company-group
		 :where (= company-group-id $s1)]
	    (id group-descriptor))))
;;(group-member-ids 571156)

(cl-defun remove-member-from-all-groups (user-id-descriptor)
  (emacsql db [:delete :from user-company-group :where (= user-id $s1)]
	   (id user-id-descriptor)))
;;(remove-member-from-all-groups (id (user-from-name "%laerer%499_1%")))

(cl-defun remove-members-from-group (group-descriptor &optional (user-role :pupil))
  (let* ((uids (mapcar #'id
		 (if user-role
		   (emacsql db
		     [:select id :from user
		       :where id :in $v1
		       :and (= user_role $r2)]
		     (coerce (group-member-ids group-descriptor) 'vector) (sstring user-role))
		   (emacsql db
		     [:select id :from user
		       :where id :in $v1]
		     (coerce (group-member-ids group-descriptor) 'vector))))))
    (when uids
      (emacsql db [:delete :from user-company-group
			   :where (= company_group_id $s1)
			   :and user_id :in $v2]
	       (id group-descriptor)
	       (coerce uids 'vector)))
    (group-member-ids group-descriptor)))
;;(remove-members-from-group (id 571156))
;;(ada-columns 'user-company-group)

(cl-defun ada-group (id-or-ids)
  "Return company-group(s) for id-or-ids"
  (if (listp id-or-ids)
    (mapcar #'ada-group id-or-ids)
    (car (emacsql db
	   [:select * :from company-group
	     :where (= id $s1)]
	   id-or-ids))))
;;(fourth (car (ada-group 571156)))

(cl-defun group-ids-from-user (user)
  (mapcar #'car (emacsql db
		  [:select company-group-id
		    :from user-company-group
		    :where (= user-id $s1)]
		  (id user))))
;;(group-ids-from-user 321210)
;;(group-ids-from-user teacher9-1)
;;(mapcar #'company-group-size (group-ids-from-user teacher9-1))

(cl-defun company-group-size (id)
  (cons id (car (emacsql db
		  [:select (funcall count id)
		    :from user-company-group
		    :where (= company-group-id $s1)]
		  id))))
;;(company-group-size 571179)

(cl-defun active-groups ()
  "Return active company-groups"
  (emacsql db
    [:select *
      :from company-group
      :where (< from-date $r1 to-date)
      ]
    (iso-date-and-time)))
;;(active-groups)

(cl-defun active-group-ids ()
  "Return ID of active company-groups"
  (mapcar #'car
    (emacsql db
      [:select id
	:from company-group
	:where (< from-date $r1 to-date)
	]
      (iso-date-and-time))))
;;(active-group-ids)

(cl-defun active-singleton-company-groups ()
  "Return groups with only one member"
  (copy 1 (mapcar #'company-group-size (active-group-ids))
    :key #'second))
;;(active-company-group-sizes)

(cl-defun group-member-p (user-id company-group-id)
  (emacsql db
    [:select *
      :from user-company-group
      :where (and (= company-group-id $s1)
		  (= user-id $s2))]
    company-group-id user-id))
;;(group-member-p (caar (user-from-name "%laerer%9_1%")) (active-group-ids))

(cl-defun active-singleton-company-groups-user (user-id-descriptor)
  (cl-loop for (company-group-id length) in (active-singleton-company-groups)
	if (group-member-p (id user-id-descriptor) company-group-id)
	collect company-group-id))
;;(active-singleton-company-groups-user teacher9-1)

(cl-defun fgroup (id &rest columns)
  "Argument COLUMNS are not yet supported"
  (tab-format (butlast (cl-loop for v in (ada-group id)
				for (k . rest ) in (ada-columns 'company-group)
				collect (list k v)))))
;;(fgroup 393484)

(provide 'ada-group)
