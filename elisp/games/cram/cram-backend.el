(require 'cram-config)

(let ((macroexp--pending-eager-loads '(skip)))
  (require 'cram-db-ui))
(require 'glicko)

(defconst +cram-default-rating+ +glicko-init-rating+)
(defconst +cram-default-user-name+ "Mats")
(defconst +cram-default-rating-window+ 200)

(defun cram-default-task-rating (operation level)
  "Eventually this method should take into account the task nature (level and operation)"
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
  (when (or update (null *cram-current-user*))
    (cram-set-current-user (or (cram-db-last-user)
				(cram-db-get-user +cram-default-user-name+))))
  *cram-current-user*)
;;(cram-current-user)

(cl-defun cram-user-ratings (&optional (user *cram-current-user*))
  (list (cram-user-rating user) (cram-user-RD user)))


;;; Current task
(defvar *cram-current-task* nil ;;(nilf *cram-current-task*)
  "This should be a tuple on the form (TASK RATING LAST-TIMESTAMP)")

(defun cram-set-current-task (task)
  (setf *cram-current-task* task))

(cl-defun cram-get-task-method (&optional (new/old-ratio 0.5))
  "The ratio of :random should be dependent on the number of tasks in :tasks"
  ;; only new tasks at this stage
  (if (< (random-float) new/old-ratio)
    :new :random))
;;(let ((n 1000000)) (/ (loop for i below n if (eql (cram-get-task-method) :new) count 1) (float n)))

(cl-defun cram-current-task ()
  "Use this method to retrive a copy of current task"
  (unless *cram-current-task*
    (setf *cram-current-task* (cram-get-task :method :next)))
  (copy-tree *cram-current-task*))
;;(cram-current-task)

(cl-defun cram-get-task (&key (method :next) (rating +cram-default-rating+))
  "Method is one of :new (default), :random, :current and :last"
  (case method
    (:last (cram-db-last-task))
    (:new (let ((task (cram-create-task :rating rating)))
	    (if (cl-notany #'null task)
	      (cram-db-insert-task task)
	      (cram-db-random-task :rating rating))))
    (:random (cram-db-random-task :rating rating))
    (:current (cram-current-task))
    (:next (cram-get-task :method (cram-get-task-method) :rating rating))
    (:update-current (cram-db-get-task (cram-task-id (cram-current-task))))))

(cl-defun cram-get-task-weird (&key (method :next) (rating +cram-default-rating+))
  "Method is one of :new (default), :random, :current and :last"
  (cram-db-insert-task (cram-create-task :rating rating)))
;;(cram-get-task :method :new)
;;(cram-get-task :method :random :rating 1500)

(cl-defun cram-draw-task (&key (method :next) (rating +cram-default-rating+))
  (setf *cram-current-task* (cram-get-task :method method :rating rating)))

(cl-defun cram-task-ratings (&optional (task (cram-current-task :last)))
  (list (cram-task-rating task) (cram-task-RD task)))


;;; other
(cl-defun cram-current-ratings (&optional (user (cram-current-user t)) (task (cram-get-task :method :current)))
  (list (cram-user-ratings user) (cram-task-ratings task)))
;;(cram-get-task :method :current)

(defun cram-operators ()
  '(:addition :substraction :multiplication :division))
;;(cram-operators)

(defun cram-random-operation ()
  (first (elt-random *cram-task-range*)))
;;(cram-random-operation)

(defun cram-random-level (operation)
  (elt-random (apply #'a-b (second (assoc operation *cram-task-range*)))))
;;(cram-random-level :substraction)

(defun cram-arguments-1 (level)
  "Returns "
  (case level
    (1 ;positive integers that adds to 10 or lower
     (let ((x (random 10)))
       (list x (random (- 11 x)))))
    (2 ;integers less than 10
     (list (random 10) (random 10)))
    (3 ;integers less than 100
     (list (random 100) (random 100)))
    (4 ;integers less than 1000
     (list (random 1000) (random 1000)))))

(defun cram-arguments (level operation)
  "Returns "
  (if (eql operation :division)
    (loop for args = (cram-arguments-1 level)
	  while (zerop (first args))
	  finally (return (list (apply #'* args) (first args))))
    (cram-arguments-1 level)))
;;(loop for i below 10000 never (= (second (cram-arguments 1 :division)) 0))

(defun cram-estimate-level (task)
  (destructuring-bind (x y) (cram-task-arguments task)
    (cond
      ;; both < 10
      ((and (< x 10) (< y 10))
       ;;... then either level 1 or 2
       (if (<= (+ x y) 10) 1 2))
      ((and (< x 100) (< y 100)) 3)
      ((and (< x 1000) (< y 1000)) 4))))
;;(mapcar #'cram-estimate-level (cram-db-tasks))

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

(defun cram-sort-task-predicate ()
  "Not in use. However an example on use of `combine<'"
  (combine< (list #'(lambda (x y) (l-explicit< x y (cram-operators))) #'cram-task-operation)
	    (list #'< #'cram-task-level)))
;;(sort (cram-db-tasks) (cram-sort-task-predicate))
;;(ld-select :tasks :columns (:operation :level))

(defun cram-group-tasks-by-type (task)
  "Not in use, but a good example on advanced use of
  #'cram-sort-task-predicate"
  (let ((tasks (cram-db-tasks))
	(pred (cram-sort-task-predicate)))
    (group (sort tasks pred) :test (lt->equal pred))))

(defun cram-estimate-task-rating (operation level)
  "Direct select, violates policy on "
  (aif (cram-db-ratings-by-type operation level)
    (average it)
    +cram-default-rating+))
;;(cram-estimate-task-rating :substraction 2)

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

(defun task-exists (operation args)
  (ld-select :tasks
    :where (and (eql :operation operation)
		(equal :arguments args))))
;;(task-exists :addition '(0 0))

;;(cl-defun cram-create-task (&key (operation :multiplication) (level 2) (rating +cram-default-rating+))
(cl-defun cram-create-task (&key (operation (cram-random-operation)) (level (cram-random-level operation)) (rating +cram-default-rating+))
  "Creates a new task (sans ID and metadata). It makes sure that
the estimated rating is within the default window around RATING.
Implementaion is kind of overkill, but makes sure that the number
of tries is quite limited"
  (setf rating +cram-default-rating+)
  (cl-flet ((task*-rating (task*) ;; since ID is lacking it is not #'sixth but
	      (or (fifth task*)
		  (error "qwe")))
	    (gentask (level &optional (RD +cram-default-RD+))
	      (loop for i below 1000
		    for args = (cram-arguments level operation)
		    unless (task-exists operation args)
		    return (list operation level args
				 (cram-calculate operation args)
				 (cram-estimate-task-rating operation level)
				 350))))
    (let ((min-rating (- rating +cram-default-rating-window+))
	  (max-rating (+ rating +cram-default-rating-window+))
	  (task* (gentask level))
	  (level-range (second (assoc operation *cram-task-range*))))
      (when task* ;; else, probably all possible problems is already in db
	(if (< (task*-rating task*) min-rating)
	  (loop for l from level to (max level (last-elt level-range))
		for task* = (gentask l)
		if (and task*
			(>= (task*-rating task*) min-rating))
		return task*
		finally return task*)
	  (loop for l from level downto (min level (first level-range))
		for task* = (gentask l)
		if (and task*
			(<= (fifth task*) max-rating))
		return task*
		finally return task*))))))
;;(loop repeat 1 collect (cram-create-task :rating 1771.5996682195228 :operation :addition))
;;(loop repeat 1 collect (cram-create-task :rating 1500))
;;(count nil (loop repeat 100 collect (cram-create-task :rating 1653.5199667679983)))

(cl-defun cram-score (task answer time-elapsed &optional (free-time 2000) (max-time 20000))
  "All TIMEs are in milliseconds."
  (let ((solution (cram-task-solution task)))
    (if (nequal answer solution)
      0
      (- 1 (min 1 (/ (max 0 (- time-elapsed free-time))
		     (coerce (- max-time free-time) 'float)))))))
;;(mapcar #'(lambda (x) (cram-score (first *cram-current-task*) (task-result (first *cram-current-task*)) x)) (a-b 0 22000 2000))

(defsubst cram-invert-score (score) (- 1 score))

;;; Ratings
(defun glicko-new-ratings (user task score &optional time)
  (let* ((uratings (cram-user-ratings user))
	 (tratings (cram-task-ratings task))
	 (res (list (glicko-rating uratings (list tratings score) time)
		    (glicko-rating tratings (list uratings (cram-invert-score score)) time))))
    (message "Calculating ratings %S ==> %S" (list uratings tratings) res)
    res))
;;(glicko-rating '(1372 350) '((1677 35) 0.04) nil)

(defun extract-ratings (user task)
  (list (list (cram-user-rating user)
	      (cram-user-RD user))
	(list (cram-task-rating task)
	      (cram-task-RD task))))

(defun cram-report-answer-strange-error (answer time-elapsed)
  (let* ((user (cram-current-user))
	 (task (cram-current-task))
	 (score (cram-score task answer time-elapsed))
	 (old-ratings (extract-ratings user task ))
	 (new-ratings (glicko-new-ratings user task score)))
    (assert (not (equal old-ratings new-ratings)))
    (destructuring-bind (updated-user updated-task)
	(apply #'cram-db-report-match
	       user task
	       (iso-date-and-time) answer time-elapsed
	       new-ratings)
      ;; Update current user and task
      ;; For some reason these updates lead to an error
      (cram-set-current-user updated-user)
      (cram-set-current-task updated-task))
    score))

(defun cram-report-answer (answer time-elapsed)
  "Return result score based on ANSWER and the TIME-ELAPSED.
Also, calculate new ratings for current user and task,
and update the current database concordingly.

Comment: the side effects here are not evident from function
name. Perhaps this function only should calculate score and new
ratings, and hand the onus of DB update to the caller"
  (let* ((user (cram-current-user))
	 (task (cram-current-task))
	 (score (cram-score task answer time-elapsed))
	 (old-ratings (extract-ratings user task))
	 (new-ratings (glicko-new-ratings user task score)))
    ;; this assert seems a bit spurious to me now
    (assert (not (equal old-ratings new-ratings)))
    (destructuring-bind (updated-user updated-task)
	(apply #'cram-db-report-match
	       user task
	       (iso-date-and-time) answer time-elapsed
	       new-ratings)
      ;; why return this from this destruct form?
      ;; it is never used. Debugging purposes?
      (list updated-user updated-task))
    score))

(defun cram-init ()
  (cram-db-init))

(defun cram-reset-all ()
  (when (yes-or-no-p "Are you sure you want to reset everything? ")
    (cram-init-database)
    (nilf *cram-current-user* *cram-current-task*)))
;;(cram-reset-all)

(defun cram-save ()
  (ld-save-database *current-database*))


;;; reports
(cl-defun cram-top-ratings (entity &key (from 0) (to 50))
  "ENTITY is either :users or :tasks"
  (cram-db-ratings entity :from from :to to))
;;(cram-top-ratings :tasks)

(provide 'cram-backend)
