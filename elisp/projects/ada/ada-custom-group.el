(require 'emacsql-mysql)
(require 'ada-group)

(cl-defun custom-groups (&optional (company-id 2585) &rest columns)
  "Return all custom groups matching COMPANY-ID.
By default, COMPANY-ID is 4, i.e. Skolen"
  (emacsql db
    (vector
     :select (column-selection columns)
     :from 'company-group
     :where '(not (is owner-user-id nil))
     :and '(= company-id $s1))
    company-id))
;;(length (custom-groups 2585))

(cl-defun find-currupt-custom-groups (&optional (company-id 2585))
  (cl-loop for (group-id teacher-id) in (custom-groups
				      company-id :id :owner-user-id)
	   unless (cl-find teacher-id (group-member-ids group-id))
	   collect group-id))
;;(find-currupt-custom-groups)

(provide 'ada-custom-group)
