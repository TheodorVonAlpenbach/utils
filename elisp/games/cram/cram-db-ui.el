;;;; Important TODO (?)
;;;; Every function calling a ld macro (ld-select, ld-update etc)
;;;; should (probably) be enclosed in an eval-when form
;;;; This is because otherwise the macros are not expanded
;;;; until eval time when the forms already (might) have been

;;;; NB! To load this buffer you must prevent eager macroexpansion. E.g.
;;;; (let ((macroexp--pending-eager-loads '(skip))) (require 'cram-db-ui))

(require 'cram-db)
(require 'cram-common)

;;;; Users
(cl-defun cram-db-insert-user (name rating)
  (ld-insert :user (list name rating)
	     :columns (list :name :rating)))
;;(cram-db-insert-user "Ludvik" '(1500 350))
;;(ld-update :user (string= :name "Ludvik") '((1500 350)) (:rating))

(cl-defun cram-db-get-user (user-designator)
  (typecase user-designator
    (string (first (ld-select :user :where (string= user-designator :name))))
    (integer (first (ld-select :user :where (= user-designator :id))))
    (t user-designator)))
;;(cram-db-get-user "Ludvik")

(cl-defun cram-user-id* (user-designator &optional check-validity)
  "Return ID of USER-DESIGNATOR. If USER-DESIGNATOR is an integer
it assumes by default that it is a valid user ID, unless
check-validity is non-nil, when it verifies that the given ID is
in fact in the DB.

Note the asterix at the end of function name. It distinguishes it
from the more primitive function `cram-user-id'"
  (if (and (integerp user-designator) (not check-validity))
    user-designator
    (cram-user-id (cram-db-get-user user-designator))))
;;(cram-user-id* -1)

(cl-defun cram-db-last-user ()
  "Return the user with last timestamp in DB"
  (min-element (ld-select :user)
	       :key #'cram-user-last-updated
	       :test #'string>=))
;;(cram-db-last-user)

(cl-defun cram-db-user-names (&key sans)
  (ld-select :user :where (not (cl-member :name (listify sans) :test #'string=))
	     :column :name))
;;(cram-db-user-names :sans "Ludvik")

(cl-defun cram-db-user-ratings (user-designator)
  (ld-select :match :where (= :user-id (cram-user-id* user-designator))
	     :columns (:user-rating ::created)))
;;(cram-db-user-ratings "Ludvik")


;;;; Problems
(cl-defun cram-db-last-problem ()
  "Low level function that 'knows' that the first row in a table is the last"
  (first (ld-table-data* :match)))
;;(cram-db-last-problem)

;;TODO
(cl-defun cram-db-random-problem-1 ()
  (awhen (ld-select :problem
	   :where (string-match *cram-ref-filter* :source-id))
    (random-elt it)))
;;(cram-db-random-problem-1)

;;; strategy for weighting
;;; number of attempts
;;; time since last error
;;; time since last response
(cl-defun cram-db-random-problem (&key (matches nil))
  "Low level function that accesses internals of a table.
TODO: Avoid crash on empty db"
  (if matches
    (let ((acc (accumulate-list
		(project-sequence matches #'cram-match-problem-id)))
	  (dacc (accumulate-list
		 (project-sequence matches #'cram-match-response)
		  :test #'string<)))
      (dprint (pp (cl-sort dacc #'> :key #'second)) 'dacc)
      (dprint (cl-sort (copy-tree acc) #'> :key #'second) 'acc>)
      (dprint (cl-sort (copy-tree acc) #'< :key #'second) 'acc<)
      (dprint (length (copy-tree acc)) 'lacc)
      (aif (ld-select :problem :where (= :id (random-weighted-element acc t)))
	(first it)
	(cram-db-random-problem-1)))
    (cram-db-random-problem-1)))
;;(cram-db-random-problem :matches (cram-get-matches))

;; not working when no questions have been asked?
(cl-defun cram-db-random-problem-old (&key (rating (car +cram-default-rating+))
				    (window +cram-default-rating-window+)
				    (idle-minutes 10))
  "Low level function that accesses internals of a table.
TODO: Avoid crash on empty db"
  (aif (ld-select :problem
	 :where (and (string-match *cram-ref-filter* :source-id)
		     (awhen rating (within it (1-sphere window rating)))
		     (time< ::updated (add-time (now) :minute (- idle-minutes)))))
    (random-elt it)
    (if (< window 100000)
      (cram-db-random-problem :rating rating :window (* 1.5 window))
      (cram-db-random-problem :rating rating :window window :idle-minutes 0))))
;;(cram-db-random-problem :rating 3000)

(cl-defun cram-db-insert-problem (problem-args)
  "problem-args is a problem without :id (and (:metadata...))"
  (ld-insert :problem problem-args))

(cl-defun cram-db-get-problem (id)
  "TODO: this doesn't work when function is loaded, when (:id) is
interpreted as a form to be evaluated later, and which then fails.
Two solutions "
  (first (ld-select :problem :where (= :id id))))
;;(cram-db-get-problem 10)

(cl-defun cram-db-problems (&optional (quarantine 1))
  "Same as ld-select :problem, but discards the
*cram-same-problem-limit* newest.
Optional QUARANTINE is not implemented."
  (if *cram-ref-filter*
    (ld-select :problem :where (string-match *cram-ref-filter* :source-id))
    (ld-select :problem)))
;;(cram-db-problems)

(cl-defun cram-db-unused-problems ()
  "Return the :problem entries that is not in any :match entry."
  (let ((ps (cram-db-problems)))
    (cl-set-difference ps (ld-select :match)
      :test #'(lambda (x y) (= (cram-problem-id x)
			       (cram-match-problem-id y))))))
;;(first (cram-db-unused-problems))
;;(length (cram-db-unused-problems))
;;(ld-select :problem :where (= :id 107))
;;(ld-select :match :where (= :problem-id 107))
;;(length (ld-select :match))
;;(cl-set-difference '((1 2) (2 4)) '((1 1) (3 3)) :test #'(lambda (x y) (= (car x) (car y))))

;;TODO
(cl-defun cram-db-ratings-by-type (operation level)
  "Returns the rating average of all problems with same :operation and :level as PROBLEM"
  (ld-select :problem
	     :where (and (eql :operation operation) (= :level level))
	     :column :rating))
;;(cram-db-ratings-by-type :substraction 2)


;;;; Matches
;;; Combined operations
(cl-defun cram-db-report-match (user problem iso-time response time score
			     new-user-rating new-problem-rating)
  "Add new match to DB, and update user and problem ratings.
It returns the updates of user and problem as a pair"
  (cl-destructuring-bind (user-id name old-user-rating &rest uargs)
      user
    (let ((problem-id (cram-problem-id problem))
	  (old-problem-rating (cram-problem-rating problem)))
      (ld-insert :match (list iso-time response time score
				user-id problem-id
				old-user-rating old-problem-rating))
      (list (ld-update :user (= (:id) user-id)
		       `(,new-user-rating) (:rating))
	    (ld-update :problem (= (:id) problem-id)
		       `(,new-problem-rating) (:rating))))))

;; Ratings
(cl-defun cram-db-rating-history (user-or-problem &key from-time to-time n)
  "Returns a list of user or problem ratings in period FROM-TIME TO-TIME"
  (cl-destructuring-bind (iso-times ratings RDs)
      (transpose (cram-get-matches
		  (if (user-p user-or-problem) :user :match)
		  user-or-problem
		  :columns '(:iso-time :user-rating :user-RD)))
    (transpose (list (push (ld-metadata user-or-problem :columns :created)
			   iso-times) 
		     (push-back (ld-column rating user-or-problem) ratings)
		     (push-back (ld-column RD user-or-problem) RDs)))))

;;(cram-db-ratings :match)
;;(ld-select :match :columns (:id (round :rating) (round :RD)) :order-by :rating)
;;(ld-select :user :columns (:name (round :rating) (round :RD)) :order-by :rating)

(provide 'cram-db-ui)
