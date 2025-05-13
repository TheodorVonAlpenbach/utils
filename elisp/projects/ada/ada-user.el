(require 'ada-mysql)

(cl-defun ada-user-p (x)
  (and (listp x)
       (> (length x) 10)
       (cl-notany #'listp x)))
;;(ada-user-p (user-from-id 321211))

(cl-defun user-from-name (name &rest columns)
  "Return a list of users matching NAME"
  (emacsql db
    (vector :select (column-selection columns)
		 :from 'user
		 :where '(like name $r1))
    name))
;;(caar (user-from-name "%celev%99_1a_1 %" :name))
;;(caar (user-from-name "%celev%99_1a_1 %" :feide-id))
;;(user-from-name "%laerer_no456326499_1%")

(cl-defun user-from-user-name (user-name &rest columns)
  "Return a list of users matching USER-NAME"
  (emacsql db
    (vector :select (column-selection columns)
	    :from 'user
	    :where '(like user-name $r1))
    user-name))
;;(car (user-from-name "%celev%99_1a_1 %"))
;;(car (user-from-name "%Haakon%"))
;;(car (user-from-name "%celev%502%"))

(cl-defun user-id-from-pseudonym (user-pseudonym)
  (caar
   (emacsql db
     [:select user-id :from user-pseudonym :where (like user-pseudonym $r1)]
     user-pseudonym)))
;;(user-id-from-pseudonym "fb0def31-9f25-4cd6-9b0d-defcbe02e815")

(cl-defun user-from-pseudonym (user-pseudonym &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'user
		 :where '(= id $s1))
	 (user-id-from-pseudonym user-pseudonym))))
;;(user-from-pseudonym "b1e464dc-b092-46ee-950a-0f9d4ef82d82")
;;(user-from-pseudonym "67e26e9c-a618-4241-829b-5d028850af58")
;;(cl-loop for up in '("21fce84d-c227-48e4-99fc-ccd64f3c905a" "44328a92-a57c-4724-8257-a27e17d71920" "336dd2be-94e8-4f95-b184-adf18d58326f") collect (user-from-pseudonym up :name))
;;(ada-columns 'user)

(cl-defun user-pseudonym (user-descriptor)
  (car (emacsql db
	  [:select * :from user-pseudonym :where (= user-id $s1)]
	  (id user-descriptor))))
;;(second (user-pseudonym (car (user-from-name "%celev%99_1a_1 %"))))

;; db['state-user-component-170'].find({userPseudonym: "40a520e7-392a-45f6-83e2-47c3923a2f52", componentUuid: "3d02de50-8e53-3c2e-b1e6-ccf84b00662c" }) 

(cl-defun user-pseudonyms (user-ids)
  (mapcar #'second
    (emacsql db
      [:select * :from user-pseudonym :where user-id :in $v1]
      user-ids)))
;;(user-pseudonyms (cl-coerce (mapcar #'string-to-integer (flatten (user-from-name "%celev%99_1a%" :id))) 'vector))

(cl-defun user-from-id (user-id-descriptor &rest columns)
  (car (emacsql db
	 (vector :select (column-selection columns)
		 :from 'user
		 :where '(= id $s1))
	 (id user-id-descriptor))))
;;(user-from-id 321211)
 
(cl-defun user (user-descriptor &rest columns)
  (if (stringp user-descriptor)
    (if (uuid-p user-descriptor)
      (apply #'user-from-pseudonym user-descriptor columns)
      (apply #'user-from-name user-descriptor columns))
    (if (ada-user-p user-descriptor)
      user-descriptor
      (apply #'user-from-id user-descriptor columns))))
;;(user "%laerer_no456326499_1%")(("321211" "1205040" "d697cb89-4695-42ca-b4b2-aad8afc8494d" "Claerer_no456326499_1 CappelenDamm" "claerer_no456326499_1@feide.no" "test@feide.no" "COMPANY_ADMIN" "2585" "aarstrinn1" "571156" "26" "2" ...))
;;(user (user "eac62d04-2488-4435-b121-87d90c4db9dc"))
;;(id (user 322181))
;;(fuser '(322190))
;;(project (user-from-name "%claerer_no456326499_1%") '(0 1 2 3 ))
;;(mapcar #'user (list "claerer_no456326499_5%" "336dd2be-94e8-4f95-b184-adf18d58326f"))
 
;;; UPDATE
(cl-defun update-user-name (user-id-descriptor user-name)
  (emacsql db
    [:update user :set (= user-name $r1) :where (= id $s2)]
    user-name user-id-descriptor))
;;(update-user-name 322168 "claerer_no456326500_4@feide.no")

;;; DELETE
(cl-defun delete-user (user-id-descriptor)
  (let* ((user (user user-id-descriptor))
	(id (id user)))
    (emacsql db [:delete :from user-company-group :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-ntp-module-codes :where (= user-id $s1)] id)
    (emacsql db [:delete :from user-pseudonym :where (= user-id $s1)] id)
    (emacsql db [:delete :from user :where (= id $s1)] id)))
;;(delete-user 322221)
;;(delete-user (user-from-name "Celev_no456326499_1a_1 CappelenDamm"))
;;(user-from-id 671726)
;;(delete-user 671726)
;;(321679 321779 321211)
;; 479-2ca8-47ad-b261-751a99c1af21, af03c96d-4f0f-4910-ada7-01d7a9dbcbff, c1720b11-7c98-4815-88a1-ab67a9e0aa88, 

(cl-defun delete-users (user-id-descriptors)
  (cl-loop for u in user-id-descriptors do (delete-user u)))

(cl-defun fuser (user-descriptor)
  (concat* (project (user user-descriptor) '(0 1 2 3 4 5 8 10)) :key #'sstring
	   :in "\n"))

 (cl-defun fuser (user-descriptor &rest columns)
  "Argument COLUMNS is not yet supported"
  (tab-format (butlast (cl-loop for v in (user user-descriptor)
				for (k . rest ) in (ada-columns 'user)
				collect (list k v)))))
;;(fuser (string-to-integer (caar (user-from-name "%elev_no456326499_1a_10%"))))
;;(fuser 321211)
;;(ada-columns 'user)

(cl-defun fusers (user-descriptors)
  (concat* user-descriptors :key #'fuser :in "\n\n"))
;;(fusers '("40a520e7-392a-45f6-83e2-47c3923a2f52" "40a520e7-392a-45f6-83e2-47c3923a2f52"))
 
(provide 'ada-user)
