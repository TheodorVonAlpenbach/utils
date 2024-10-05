(require 'ada-mysql)

;; (unless db (setf db (ada-mysql-connect :ada_test)))
(let ((group-ids (active-groups)))
    (emacsql db [:select id :from user-company-group
		  :where (= company-group-id $s1)]
	     (first (company-group group-from-descriptor))))

(cl-defun company-id-in-user-company-group-superfluous-p ()
  "The result is NIL. The company-id is indeed superfluous!"
  (cl-loop for (group-id company-id . rest) in (active-groups)
	if (emacsql db [:select * :from user-company-group
			      :where (and (= company-group-id $s1)
					  (not (= company-id $s2)))]
		    group-id company-id)
	collect it))
;;(company-id-in-user-company-group-superfluous-p)

(cl-defun find-merge-users (group-from-descriptor)
  (emacsql db [:select * :from user :where (= company-group-id $s1)]
	   (first (company-group group-from-descriptor))))
;;(mapcar #'fourth (find-merge-users 571184))
;;(group-members 571184)
;;(group-members 571156)

(cl-defun merge-groups (group-from-descriptor group-to-descriptor)
  "Merge GROUP-FROM-DESCRIPTOR to GROUP-TO-DESCRIPTOR.
This operation does the following:
1. identify all users with company-group-id matching GROUP-FROM-DESCRIPTOR
2. replace these id's with the company-group-id from GROUP-TO-DESCRIPTOR
3. delete all user-company-group's matching GROUP-FROM-DESCRIPTOR
4. delete GROUP-FROM-DESCRIPTOR
"
  (let ((group-from (company-group group-from-descriptor))
	(group-to (company-group group-to-descriptor)))
    (emacsql db [:update user :set (= company-group-id $s1) :where (= company-group-id $s2)]
	     (id group-to) (id group-from))
    (remove-members-from-group group-from)
    (emacsql db [:delete :from company-group :where (= id $s1)] (id group-from))))
;;(merge-groups 571184 571156)

(provide 'merge-groups)
