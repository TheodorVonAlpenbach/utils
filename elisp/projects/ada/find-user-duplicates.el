(require 'ada-user)
(require 'ada-group)

(defun user-name-normal-form (user-name)
  (apply #'format "%s@%s"
	 (string-match*
	     "^\\(feide:\\)?\\([^@]+\\)@\\(\\([tesurp]+users\\)?\\.\\)?\\(.*\\)$"
	   user-name :num '(2 5))))
;;(equal-elements (mapcar #'user-name-normal-form (cartesian-product '(("feide:" "") ("qwe@") ("spusers." "testusers." "") ("feide.no")) #'concat)))
;;(user-name-normal-form "qwe@feide.no")

(cl-defun user-ids-from-seed (seed &optional (columns '(:id :salto-id :feide-id :user-name :company-id :stamp-updated)))
  "For SEED, see `find-superfluous-users'."
  (remove-duplicates
      (mapcar #'id
	(typecase seed
	  (string (user-from-user-name seed :id))
	  (cons (loop for u in seed collect (user u :id)))
	  (number (user-ids-from-seed (mapcan #'group-member-ids (user-groups seed))))))))
;;(loop for x in '(321678 (321678) "c%_no456326%") collect (user-ids-from-seed x))

(cl-defun users-from-seed (seed &optional (columns '(:id :salto-id :feide-id :user-name :company-id :stamp-updated)))
  "SEED is either
  · list of user IDs: corresponds directly to the target users
  · a regexp: matches USER-NAME of target users
  · a user ID: target users will be all users in the same groups as this user."
  (loop for u in (user-ids-from-seed seed) collect (apply #'user u columns)))
;;(loop for x in '(321678 (321678) "c%_no456326%") collect (users-from-seed x))

(cl-defun find-user-name-duplicates-1 (&optional (seed "c%_no456326%"))
  "Return a list of user fragment pairs grouped by duplicity.
For SEED, see `user-ids-from-seed."
  (cl-loop for (id salto-id feide-id user-name company-id . rest) in
	   (users-from-seed seed)
	   collect
	   (append (list id (ada-sstring salto-id) (ada-sstring feide-id)
			 (ada-sstring user-name) company-id)
		   rest)))
;;(find-user-name-duplicates-1 "c%_no456326502_3%")
;;(find-user-name-duplicates-1 "c%_no456326499_1a_1 %")

(cl-defun find-user-name-duplicates (&optional (seed "c%_no456326%"))
  "Return a list of user fragment pairs grouped by duplicity.
For SEED, see `user-ids-from-seed."
  (remove-if (bind #'< 2)
    (equivalence-class-with-key (find-user-name-duplicates-1 seed)
      :key #'(lambda (x) (list (user-name-normal-form (fourth x))
			       (fifth x)))
      :test #'equal)
    :key (compose #'length #'second)))
;;(find-user-name-duplicates 321678)
;;(find-user-name-duplicates "%")
;;(find-user-name-duplicates "c%_no456326499_1a_1 %")
;;(cadadr (car (find-user-name-duplicates)))

(defun superfluous-users (k class)
  ;; (rest (cl-sort class #'string< :key #'sixth))
  (copy "NULL" class :test #'string= :key #'second))
;;(superfluous-users '("c" 123) '(("1" "NULL" "f" "un" "c" "date1") ("0" "s" "f" "un" "c" "date2")))
;;(superfluous-users '("d" 123) '(("1" "s" "f" "un" "c" "date1") ("0" "s" "f" "un" "c" "date2")))
;;(cl-remove "0" '("0"))

(cl-defun find-superfluous-users (&optional (seed "c%_no456326%"))
  "Return a list of user fragments prone to deletion due to duplicity.
"
  (cl-loop for (k class) in (find-user-name-duplicates seed)
	   append (superfluous-users k class)))
;;(find-superfluous-users 56968)
;;(find-superfluous-users 321678)
;;(find-superfluous-users "%c%_no456326502_%")
;;(length (find-superfluous-users "%c%_no456326502_1a%"))
;;(concat* (find-superfluous-users) :key #'fourth)
;;(find-superfluous-users "%")
;;(user-from-name "Chris Arthur Wiig")
;;(user 727148)

;;(delete-user x 726857)
;;(delete-users x wiig-ids)

(setf wiig (find-superfluous-users 56968))
(setf wiig-ids (mapcar #'id wiig))
(setf wiig-member-ids (mapcan #'group-member-ids (user-groups 56968)))
(mapcar #'length (list wiig-ids wiig-member-ids))
(cl-intersection wiig-ids wiig-member-ids)
(length (cl-intersection wiig-ids wiig-member-ids))
(concat* wiig-member-ids :in "," :pre "(" :suf ")" :key #'sstring)
"(56968,312983,575410,585717,585630,585716,601081,585662,585682,585622,585664,585723,585618,4696,311795,585643,585621,585624,584171,585677,585628,585629,726857,17916,575410,56968,312983,620062,597259,592307,592293,620061,597820,597279,592300,597260,592246,620068,592265,592343,592342,597315,592288,592299,592306,592346,4696,311795,592273,726857,17916,56968,312983,575410,620100,620129,620183,620159,620098,620166,620135,620144,620154,620088,620141,620163,620148,623844,620142,4696,311795,726857,17916,620131,56968,312983,575410,620109,620104,695475,620096,612869,4696,311795,612865,620091,717895,620084,717908,717943,726857,748728,17916,620085,828837,726857,727144,727145,727146,727147,727148,727149,727150,727151,727152,727153,727154,727155,727156,727157,727158,727159,727160,727161,727162,727163,56968,597820,592346,592300,592342,592307,597259,597279,592306,592246,592299,592288,597260,597315,620062,592293,620068,592265,620061,592273,592343,726857,727164,727165,727166,727167,727168,727169,727170,727171,727172,727173,727174,727175,727176,56968,695475,620096,717895,620109,612869,620104,748728,620091,717908,620084,717943,612865,620085,828837,726857,727177,727178,727180,727181,727182,727183,727184,727185,727186,727187,727188,727189,727190,727191,56968,585717,585643,585622,585618,585662,585630,584171,601081,585664,585723,585629,585621,585624,585677,585682,585716,585628,620109,620104,695475,620096,612869,612865,620091,717895,620084,717908,717943,56968,748728,620085,726857,828837,620062,597259,592307,592293,620061,597820,597279,592300,597260,592246,620068,592265,592343,592342,597315,592288,592299,592306,592346,592273,56968,726857,620100,620129,620183,620159,620098,620166,620135,620144,620154,620088,620141,620163,620148,623844,620142,56968,620131,726857,585717,585630,585716,601081,585662,585682,585622,585664,585723,585618,585643,585621,585624,584171,585677,585628,585629,56968,726857)"
(concat* (sort wiig-ids #'<) :in "," :pre "(" :suf ")" :key #'sstring)
"(726857,727144,727145,727146,727147,727148,727149,727150,727151,727152,727153,727154,727155,727156,727157,727158,727159,727160,727161,727162,727163,727164,727165,727166,727167,727168,727169,727170,727171,727172,727173,727174,727175,727176,727177,727178,727180,727181,727182,727183,727184,727185,727186,727187,727188,727189,727190,727191)"
(provide 'find-user-duplicates)
