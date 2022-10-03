(require 'ada-mysql)

(defun ada-user-p (x)
  (and (listp x)
       (> (length x) 10)
       (cl-notany #'listp x)))
;;(ada-user-p (user-from-id 321211))

(defun user-from-name (name &rest columns)
  "Return a list of users matching NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'user
		 :where '(like name $r1))
    name))
;;(user-from-name "%elev_no456326499_1a_1 %")

(defun user-from-user-name (user-name &rest columns)
  "Return a list of users matching USER-NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'user
		 :where '(like user-name $r1))
    user-name))
;;(caar (user-from-name "%laerer%99_5%"))

(defun user-id-from-pseudonym (user-pseudonym)
  (string-to-integer
   (caar (emacsql db
	   [:select user-id :from user-pseudonym :where (= user-pseudonym $r1)]
	   user-pseudonym))))
;;(user-id-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun user-from-pseudonym (user-pseudonym &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'user
		 :where '(= id $s1))
	 (user-id-from-pseudonym user-pseudonym))))
;;(user-from-pseudonym "336dd2be-94e8-4f95-b184-adf18d58326f")

(defun user-pseudonym (user-descriptor)
  (caar (emacsql db
	  [:select user-pseudonym :from user-pseudonym :where (= user-id $s1)]
	  (id user-descriptor))))
;;(user-pseudonym 321210)

(defun user-from-id (user-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'user
		 :where '(= id $s1))
	 (id user-id-descriptor))))
;;(user-from-id 321211)
 
(defun user (user-descriptor &rest columns)
  (if (stringp user-descriptor)
    (apply #'user-from-pseudonym user-descriptor columns)
    (if (ada-user-p user-descriptor)
      user-descriptor
      (apply #'user-from-id user-descriptor columns))))
;;(user (user "eac62d04-2488-4435-b121-87d90c4db9dc"))
;;(user-from-id 321211)
;;(user 321211)
;;(user '(321211))
;;(project (user-from-name "%claerer_no456326499_1%") '(0 1 2 3 ))
;;(mapcar #'user (list "claerer_no456326499_5%" "336dd2be-94e8-4f95-b184-adf18d58326f"))
 
;;; UPDATE
(defun update-user-name (user-id-descriptor user-name)
  (emacsql db
    [:update user :set (= user-name $r1) :where (= id $s2)]
    user-name user-id-descriptor))
;;(update-user-name 322168 "claerer_no456326500_4@feide.no")

;;; DELETE
(defun delete-user (user-id-descriptor)
  (let ((id (id user-id-descriptor)))
    (emacsql db [:delete :from user-company-group :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-ntp-module-codes :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-pseudonym :where (= user-id $s1)] id)
    (emacsql db [:delete :from user :where (= id $s1)] id)))
;;(delete-user (user-from-name "Celev_no456326499_7a_5 CappelenDamm"))
;;(delete-user 321679)
;;(321679 321779 321211)

(defun delete-users (user-id-descriptors)
  (loop for u in user-id-descriptors do (delete-user u)))

(defun fuser (user-descriptor)
  (concat* (project (user user-descriptor) '(0 1 2 3 4 5 8 10)) :key #'sstring
	   :in "\n"))
;;(fuser 321211)
 
(defun fusers (user-descriptors)
  (concat* user-descriptors :key #'fuser :in "\n\n"))
;;(fusers '("40a520e7-392a-45f6-83e2-47c3923a2f52" "40a520e7-392a-45f6-83e2-47c3923a2f52"))
 
(provide 'ada-user)
