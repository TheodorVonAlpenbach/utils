(require 'cram-config)

(let ((macroexp--pending-eager-loads '(skip)))
  (require 'cram-db-ui))
(require 'glicko)

(defconst +cram-default-rating+ +glicko-init-rating+)
(defconst +cram-default-user-name+ "Mats")
(defconst +cram-default-rating-window+ 200)

(defun cram-default-problem-rating (operation level)
  "Eventually this method should take into account the problem nature (level and operation)"
  +cram-default-rating+)

(defvar *cram-current-user* nil ;;(nilf *cram-current-user*)
  "This should be a tuple on the form (USER-NAME RATING LAST-TIMESTAMP)")

(cl-defun cram-add-user (name &optional (rating +cram-default-rating+))
  (cram-db-insert-user name rating))

(defun cram-set-current-user (user)
  (setf *cram-current-user* (if (stringp user) (cram-db-get-user user) user)))

(defun cram-current-user-old (&optional update) 
  (when (or update (null *cram-current-user*))
    (cram-set-current-user (cram-db-get-user +cram-default-user-name+)))
  (unless *cram-current-user*
    (cram-set-current-user (cram-db-last-user)))
  *cram-current-user*)

(defun cram-current-user (&optional update) 
  (when *current-database*
    (when (or update
	      (null *cram-current-user*))
      (cram-set-current-user (or (cram-db-last-user)
				 (cram-db-get-user +cram-default-user-name+))))
    *cram-current-user*))
;;(cram-current-user t)

;;; Current problem
(defvar *cram-current-problem* nil ;;(nilf *cram-current-problem*)
  "This should be a tuple on the form (PROBLEM RATING LAST-TIMESTAMP)")

(defun cram-set-current-problem (problem)
  (setf *cram-current-problem* problem))

(cl-defun cram-get-problem-method (&optional (new/old-ratio 0.5))
  "The ratio of :random should be dependent on the number of problems in :problems"
  ;; only new problems at this stage
  (if (< (random-float) new/old-ratio)
    :new :random))
;;(let ((n 1000000)) (/ (loop for i below n if (eql (cram-get-problem-method) :new) count 1) (float n)))

(cl-defun cram-get-worst-problem ()
  "Use this method to retrive a copy of current problem"
  (let ((ps (cram-db-problems)))
    (if (< (length ps) *cram-same-problem-limit*)
      (last-elt ps)
      (min-element
       (nthcdr *cram-same-problem-limit*
	       (cl-sort ps #'string> :key #'cram-problem-updated))
       :key #'cram-problem-rating-e :test #'>))))
;;(cram-get-worst-problem)

(cl-defun cram-get-cram-problem (&optional (user (cram-current-user)))
  "Return the most recent problem that has not been solved the last three times"
  (let ((ms (ld-select :match :where (= (cram-user-id user) :user-id))))
    (loop for m in (mapcar (bind #'head 3 1)
		     (group (cl-sort ms #'> :key #'cram-match-problem-id)
		       :key #'cram-match-problem-id :test #'=))
	  for p = (and m
		       (ld-select :problem :where (= (cram-match-problem-id m)
						     :problem-id)))
	  collect (cram-problem-answer p))
    ))
;;(setf qwe (cram-get-cram-problem))

(cl-defun cram-current-problem ()
  "Use this method to retrive a copy of current problem"
  (unless *cram-current-problem*
    (setf *cram-current-problem* (cram-get-problem :method :next)))
  (copy-tree *cram-current-problem*))
;;(cram-current-problem)

(cl-defun cram-get-problem (&key (method :worst) (rating +cram-default-rating+))
  "Return a problem from database, or a create a new problem depending on method.
If METHOD is
  :NEW     return a new problem a created with `cram-create-new',
  :LAST    return the last problem the current user met in a match,
  :RANDOM  return a problem selected at random from the database,
  :CURRENT return the problem stored in *cram-current-problem*,
  :RATING  return the problem closest to a certain rating, and
  :WORST   return the most difficult problem in database.

Many of these methods take additional parameters that can modify
the selection strategy somewhat. For instance, :RANDOM avoids the last problems presented to the user, see "
  (case method
    (:last (cram-db-last-problem))
    (:new (let ((problem (cram-create-problem :rating rating)))
	    (if (cl-notany #'null problem)
	      (cram-db-insert-problem problem)
	      (cram-db-random-problem :rating rating))))
    (:random (cram-db-random-problem :rating rating))
    (:current (cram-current-problem))
    (:next (cram-get-problem :method (cram-get-problem-method) :rating rating))
    (:worst (cram-get-worst-problem))
    (:cram (cram-get-cram-problem))
    (:update-current (cram-db-get-problem (cram-problem-id (cram-current-problem))))))

(cl-defun cram-get-problem-weird (&key (method :worst)
				    (rating +cram-default-rating+)))
;;(cram-get-problem :method :new)
;;(cram-get-problem :method :random :rating 1500)

(cl-defun cram-draw-problem (&key (method :worst) (rating +cram-default-rating+))
  (setf *cram-current-problem* (cram-get-problem :method method :rating rating)))

;;; other
(cl-defun cram-current-ratings (&optional
				(user (cram-current-user t))
				(problem (cram-get-problem :method :current)))
  "Return ratings of current user and problem as a pair of pairs.
Why was cram-current-user called with t (update arg)?."
  (list (cram-user-rating user)
	(cram-problem-rating problem)))
;;(cram-current-ratings)

(defun cram-operators ()
  '(:addition :substraction :multiplication :division))
;;(cram-operators)

(defun cram-random-operation ()
  (first (elt-random *cram-problem-range*)))
;;(cram-random-operation)

(defun cram-random-level (operation)
  (elt-random (apply #'a-b (second (assoc operation *cram-problem-range*)))))
;;(cram-random-level :substraction)

(defun combine< (&rest predicates)
  (lexical-let ((preds predicates))
    #'(lambda (x y)
	(loop for (pred key) in preds
	      for x* = (funcall key x)
	      for y* = (funcall key y)
	      if (funcall pred x* y*) return t
	      if (funcall pred y* x*) return nil))))
;;(funcall (combine< (list #'< #'first) (list #'string< #'second)) '(1 "a") '(1 "b"))

(defun lt->equal (lt)
  (lexical-let ((lt lt))
    #'(lambda (x y) (nor (funcall lt x y) (funcall lt y x)))))
;;(funcall (lt->equal #'<) 1 1)

(defun cram-sort-problem-predicate ()
  "Not in use. However an example on use of `combine<'"
  (combine< (list #'(lambda (x y)
		      (l-explicit< x y (cram-operators)))
		  #'cram-problem-operation)
	    (list #'< #'cram-problem-level)))
;;(sort (cram-db-problems) (cram-sort-problem-predicate))
;;(ld-select :problems :columns (:operation :level))

(defun cram-group-problems-by-type (problem)
  "Not in use, but a good example on advanced use of
  #'cram-sort-problem-predicate"
  (let ((problems (cram-db-problems))
	(pred (cram-sort-problem-predicate)))
    (group (sort problems pred) :test (lt->equal pred))))

(defun cram-estimate-problem-rating (operation level)
  "Direct select, violates policy on "
  (aif (cram-db-ratings-by-type operation level)
    (average it)
    +cram-default-rating+))
;;(cram-estimate-problem-rating :substraction 2)

(defun cram-init-rating (&optional operation level)
  +cram-default-rating+)

(defun cram-calculate (operation args)
  (apply (cram-operator operation) args))

(defun cram-operator (operation)
  (case operation
    (:addition #'+)
    (:substraction #'-)
    (:multiplication #'*)
    (:division #'/)))
;;(mapcar #'cram-operator '(:addition :substraction :multiplication :division))

(defun problem-exists (operation args)
  (ld-select :problems
    :where (and (eql :operation operation)
		(equal :arguments args))))
;;(problem-exists :addition '(0 0))

;;(cl-defun cram-create-problem (&key (operation :multiplication) (level 2) (rating +cram-default-rating+))
(cl-defun cram-create-problem (&key (operation (cram-random-operation)) (level (cram-random-level operation)) (rating +cram-default-rating+))
  "Creates a new problem (sans ID and metadata). It makes sure that
the estimated rating is within the default window around RATING.
Implementaion is kind of overkill, but makes sure that the number
of tries is quite limited"
  (setf rating +cram-default-rating+)
  (cl-flet ((problem*-rating (problem*) ;; since ID is lacking it is not #'sixth but
	      (or (fifth problem*)
		  (error "qwe")))
	    (genproblem (level &optional (RD +cram-default-RD+))
	      (loop for i below 1000
		    for args = (cram-arguments level operation)
		    unless (problem-exists operation args)
		    return (list operation level args
				 (cram-calculate operation args)
				 (cram-estimate-problem-rating operation level)
				 350))))
    (let ((min-rating (- rating +cram-default-rating-window+))
	  (max-rating (+ rating +cram-default-rating-window+))
	  (problem* (genproblem level))
	  (level-range (second (assoc operation *cram-problem-range*))))
      (when problem* ;; else, probably all possible problems is already in db
	(if (< (problem*-rating problem*) min-rating)
	  (loop for l from level to (max level (last-elt level-range))
		for problem* = (genproblem l)
		if (and problem*
			(>= (problem*-rating problem*) min-rating))
		return problem*
		finally return problem*)
	  (loop for l from level downto (min level (first level-range))
		for problem* = (genproblem l)
		if (and problem*
			(<= (fifth problem*) max-rating))
		return problem*
		finally return problem*))))))
;;(loop repeat 1 collect (cram-create-problem :rating 1771.5996682195228 :operation :addition))
;;(loop repeat 1 collect (cram-create-problem :rating 1500))
;;(count nil (loop repeat 100 collect (cram-create-problem :rating 1653.5199667679983)))

(require 'mb-utils-strings)
(defun expand-alternatives (pattern)
  "Expand parentheses alternatives in solution string.
The rules can be summarized in these examples:

\"a\"         -> (\"a\")
\"(a)\"       -> (\"(a)\" \"a\" \"\") and issue a warning!
\"(a) b\"     -> (\"(a) b\" \"a b\" \"b\")
\"(a) (c) b\" -> (\"(a) (c) b\" \"(a) c b\" \"(a) b\"
		\"a (c) b\"   \"a c b\"   \"a b\"
		\"(c) b\"     \"c b\"     \"b\")"
  (loop for x in (combine (loop for x in (read-whole-string pattern)
				for s = (format "%S" x)
				collect (if (listp x)
					  (list s (substring s 1 -1) nil)
					  (list s))))
	collect (string-trim (concat* x :in " "))))
;;(expand-alternatives "b")
;;(expand-alternatives "(a) (c) b")
;;(expand-alternatives "(a) b")
;;(combine '((a nil) (b nil)))

(defun cram-expand-alternatives (problem)
  "Collect expansions of PROBLEM solution and all alternatives."
  (mapcar #'expand-alternatives
    (cons (cram-problem-answer problem)
	  (awhen (cram-problem-alternatives problem)
	    (split-string it "|")))))
;;(cram-expand-alternatives (car (ld-select :problem :where (string-match* "konstant" :answer))))
;;(ld-select :problem)

(defun cram-correct-response-p (problem response)
  "Return nil if and only if RESPONSE is incorrect according to PROBLEM.
RESPONSE is correct if it
1. matches SOLUTION perfectly
2. matches SOLUTION with parenthesis characters removed
3. matches SOLUTION with parentheses removed entirely.

where SOLUTION is a column in PROBLEM. Whitespace repetitions are
ignored both in SOLUTION and RESPONSE. Also these rules apply to
each item in the ALTERNATIVES column of PROBLEM.

See also cram-extract-alternatives.
"
  (cl-member response (flatten (cram-expand-alternatives problem))
	     :test #'string=))
;;(cram-correct-response-p (car (ld-select :problem :where (string-match "Klaus" :answer))) "Doldinger")

(cl-defun cram-score (problem response time-elapsed
			      &optional (free-time 5000) (max-time 30000))
  "All TIMEs are in milliseconds."
  (let ((answer (cram-problem-answer problem)))
    (if (cram-correct-response-p problem response)
      (- 1 (min 1 (/ (max 0 (- time-elapsed free-time))
		     (coerce (- max-time free-time) 'float))))
      0)))
;;(mapcar #'(lambda (x) (cram-score (first *cram-current-problem*) (problem-result (first *cram-current-problem*)) x)) (a-b 0 22000 2000))

(defsubst cram-invert-score (score) (- 1 score))

;;; Ratings
(defun glicko-new-ratings (user problem score &optional time)
  "Calculate new ratings for USER and PROBLEM given SCORE.
Optional TIME is the amount of days since the last time problem
was SOLVED."
  (let* ((uratings (cram-user-rating user))
	 (tratings (cram-problem-rating problem))
	 (res (list (glicko-rating uratings (list tratings score) time)
		    (glicko-rating tratings
				   (list uratings (cram-invert-score score))
				   time))))
	  (message "Calculating ratings %S ==> %S" (list uratings tratings) res)
	   res))
;;(glicko-new-ratings (cram-current-user) (cram-current-problem) 0)
;;(glicko-rating '(1776 47) '((1413 217) .85) nil)
;;(glicko-rating '(1413 217) '((1776 47) .15) nil)

(defun extract-ratings (user problem)
  "Return a pair of ratings. Superfluous util?!"
  (list (cram-user-rating user)
	(cram-problem-rating problem)))

(defun cram-report-response-strange-error (response time-elapsed)
  (let* ((user (cram-current-user))
	 (problem (cram-current-problem))
	 (score (cram-score problem response time-elapsed))
	 (old-ratings (extract-ratings user problem))
	 (new-ratings (glicko-new-ratings user problem score)))
    (assert (not (equal old-ratings new-ratings)))
    (destructuring-bind (updated-user updated-problem)
	(apply #'cram-db-report-match
	       user problem
	       (iso-date-and-time) response time-elapsed
	       new-ratings)
      ;; Update current user and problem
      ;; For some reason these updates lead to an error
      (cram-set-current-user updated-user)
      (cram-set-current-problem updated-problem))
    score))

(defun cram-report-response (response time-elapsed)
  "Return result score based on RESPONSE and the TIME-ELAPSED.
Also, calculate new ratings for current user and problem,
and update the current database concordingly.

Comment: the side effects here are not evident from function
name. Perhaps this function only should calculate score and new
ratings, and hand the onus of DB update to the caller"
  (let* ((user (or (cram-current-user)
		   (cram-register-new-user)))
	 (problem (cram-current-problem))
	 (score (cram-score problem response time-elapsed))
	 (old-ratings (extract-ratings user problem))
	 (new-ratings (glicko-new-ratings user problem score)))
    ;; this assert seems a bit spurious to me now
    (assert (not (equal old-ratings new-ratings)))
    (destructuring-bind (updated-user updated-problem)
	(apply #'cram-db-report-match
	       user problem
	       (iso-date-and-time :with-seconds t)
	       response
	       time-elapsed
	       new-ratings)
      ;; why return this from this destruct form?
      ;; it is never used. Debugging purposes?
      (list updated-user updated-problem))
    score))

(defun cram-init (&optional force)  
  (cram-db-init force))

(defun cram-reset-all ()
  (when (yes-or-no-p "Are you sure you want to reset everything? ")
    (cram-init-database t)
    (nilf *cram-current-user* *cram-current-problem*)))
;;(cram-reset-all)

(defun cram-save ()
  (ld-save-database *current-database*))


;;; reports
(cl-defun cram-top-ratings (entity &key (from 0) (to 50))
  "ENTITY is either :users or :problems"
  (cram-db-ratings entity :from from :to to))
;;(cram-top-ratings :problems)

(provide 'cram-backend)
