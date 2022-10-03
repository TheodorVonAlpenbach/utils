(require 'ada-user)

(defun user-name-normal-form (user-name)
  (apply #'format "%s@%s"
	 (string-match*
	     "^\\(feide:\\)?\\([^@]+\\)@\\([tesurp]+users\\.\\)?\\(.*\\)$"
	   user-name :num '(2 4))))
;;(equal-elements (mapcar #'user-name-normal-form (cartesian-product '(("feide:" "") ("qwe@") ("spusers." "testusers." "") ("feide.no")) #'concat)))

(cl-defun find-user-name-duplicates-1 (&optional (name-regexp "c%_no456326%"))
  "Return a list of user fragment pairs grouped by duplicity."
  (loop for (id salto-id feide-id user-name company-id . rest) in
	(user-from-name
	 name-regexp :id :salto-id :feide-id
	 :user-name :company-id :stamp-updated)
	collect
	(append (list id (ada-sstring salto-id) (ada-sstring feide-id)
		      (ada-sstring user-name) company-id)
		rest)))
;;(find-user-name-duplicates-1 "c%_no456326499_1a_1 %")

(cl-defun find-user-name-duplicates (&optional (name-regexp "c%_no456326%"))
  "Return a list of user fragment pairs grouped by duplicity."
  (remove-if (bind #'< 2)
    (equivalence-class-with-key (find-user-name-duplicates-1)
      :key #'(lambda (x) (list (user-name-normal-form (fourth x)) (fifth x)))
      :test #'equal)
    :key (compose #'length #'second)))
;;(find-user-name-duplicates "c%_no456326499_1a_1 %")
;;(cadadr (car (find-user-name-duplicates)))
;;(delete-user (cadadr (car (find-user-name-duplicates))))

(defun superfluous-users (k class)
  (awhen (cl-remove (car k) class :test #'string= :key #'fourth)
    (if (/= (length it) (length class))
      it
      ;; else do nothing; handle this case manually
      (warn "No user in class has the canonical user name %s" (car k))
      nil)))
;;(superfluous-users '("c" 123) '((0 1 2 "a") (0 1 2 "c")))

(cl-defun find-superfluous-users (&optional (name-regexp "c%_no456326%"))
  "Return a list of user fragments prone to deletion due to duplicity."
  (loop for (k class) in (find-user-name-duplicates name-regexp)
	append (superfluous-users k class)))
;;(find-superfluous-users "c%_no456326499_1a_1 %")

;;(delete-users (find-superfluous-users))
;;(delete-user 322178)

(provide 'find-user-duplicates)
