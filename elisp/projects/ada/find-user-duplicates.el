(require 'ada-mysql)

(unless db
  (setf db (ada-mysql-connect :ada_test)))

(defun find-user-duplicates ()
  "Return a list of user fragment pairs grouped by duplicity."
  (copy-if #'(lambda (x) (> (length x) 1))
    (group (cl-sort (mapcar (bind #'project (list 0 (compose #'sstring #'fourth) 1))
		      (user-from-name "%elev_no%"))
	     #'istring< :key #'second)
      :test #'istring= :key #'second)))
;;(length (find-user-duplicates))

(defun superfluous-user (u1 u2)
  (if (eql (third u1) 'NULL)
    (when (not (eql (third u2) 'NULL))
      u1)
    (if (eql (third u2) 'NULL)
      u2)))

(defun find-superfluous-users ()
  "Return a list of user fragments prone to deletion due to duplicity."
  (loop for (u1 u2) in (find-user-duplicates)
	if (superfluous-user u1 u2) collect it into to-delete
	else collect (list u1 u2) into weird
	finally return (list to-delete weird)))
;;(find-superfluous-users)

(defun delete-superfluous-users ()
  "Delete all user duplicates from current ada db."
  (loop for u in (car (find-superfluous-users)) do (delete-user u)))
;;(delete-superfluous-users)

(require 'sql-utils)
;;(sql-list (delete-superfluous-users))

(provide 'find-user-duplicates)

