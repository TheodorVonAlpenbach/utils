(require 'ada-mysql)

(defun user-from-name (name)
  (emacsql db
    [:select * :from user :where (like name $r1)]
    name))
;;(caar (user-from-name "%laerer%99_5%"))

(defun user-id-from-pseudonym (user-pseudonym)
  (caar (emacsql db [:select user-id :from user-pseudonym :where (= user-pseudonym $r1)]
		 user-pseudonym)))
;;(user-id-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun user-from-pseudonym (user-pseudonym)
  (emacsql db [:select * :from user :where (= id $s1)]
	   (user-id-from-pseudonym user-pseudonym)))
;;(user-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun user-from-id (user-id-descriptor)
  (emacsql db [:select * :from user :where (= id $s1)] (id user-id-descriptor)))
;;(user "336dd2be-94e8-4f95-b184-adf18d58326f")
 
(defun user (user-descriptor)
  (if (stringp user-descriptor)
    (if (find ?- user-descriptor)
      (user-from-pseudonym user-descriptor)
      (user-from-name user-descriptor))
    (emacsql db [:select * :from user :where (= id $s1)] (id user-descriptor))))
;;(user 321208)
;;(project (user "%claerer_no456326499_5%") '(0 1 2 3 ))
;;(mapcar #'user (list "claerer_no456326499_5%" "336dd2be-94e8-4f95-b184-adf18d58326f"))
 
(defun delete-user (user-id-descriptor)
  (let ((id (id user-id-descriptor)))
    (emacsql db [:delete :from user-company-group :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-ntp-module-codes :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-pseudonym :where (= user-id $s1)] id)
    (emacsql db [:delete :from user :where (= id $s1)] id)))
;;(delete-user (user-from-name "Celev_no456326499_7a_5 CappelenDamm"))
;;(delete-user 321679)
;;(321679 321779 321211)

(defun fuser (user-descriptor)
  (concat* (project (car (user user-descriptor)) '(0 1 2 3 4 8 10)) :key #'sstring
	   :in "\n"))
;;(fuser 321860)
;;(project (user "%claerer_no456326499_5%") '(0 1 2 3 ))
;;(mapcar #'user (list "claerer_no456326499_5%" "336dd2be-94e8-4f95-b184-adf18d58326f"))
 
(provide 'ada-user)
