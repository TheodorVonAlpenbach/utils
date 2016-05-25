(require 'maths-config)
(let ((macroexp--pending-eager-loads '(skip))) (require 'maths-db-ui))
(require 'glicko)

(defconst +maths-default-rating+ (first +glicko-init-rating+))
(defconst +maths-default-RD+ (second +glicko-init-rating+))
(defconst +maths-default-user-name+ "Mats")
(defconst +maths-default-rating-window+ 200)

(defun maths-default-task-rating (operation level)
  "Eventually this method should take into account the task nature (level and operation)"
  +maths-default-rating+)

(defvar *maths-current-user* nil ;;(nilf *maths-current-user*)
  "This should be a tuple on the form (USER-NAME RATING LAST-TIMESTAMP)")

(cl-defun maths-add-user (name age &optional (rating +maths-default-rating+) (RD +maths-default-RD+))
  (maths-db-insert-user name age rating RD))

(defun maths-set-current-user (user)
  (setf *maths-current-user* (if (stringp user) (maths-db-get-user user) user)))

(defun maths-current-user-old (&optional update) 
  (when (or update (null *maths-current-user*))
    (maths-set-current-user (maths-db-get-user +maths-default-user-name+)))
  (unless *maths-current-user*
    (maths-set-current-user (maths-db-last-user)))
  *maths-current-user*)

(defun maths-current-user (&optional update) 
  (when (or update (null *maths-current-user*))
    (maths-set-current-user (or (maths-db-last-user)
				(maths-db-get-user +maths-default-user-name+))))
  *maths-current-user*)
;;(maths-current-user)

(cl-defun maths-user-ratings (&optional (user *maths-current-user*))
  (list (maths-user-rating user) (maths-user-RD user)))


;;; Current task
(defvar *maths-current-task* nil ;;(nilf *maths-current-task*)
  "This should be a tuple on the form (TASK RATING LAST-TIMESTAMP)")

(defun maths-set-current-task (task)
  (setf *maths-current-task* task))

(cl-defun maths-get-task-method (&optional (new/old-ratio 0.5))
  "The ratio of :random should be dependent on the number of tasks in :tasks"
  ;; only new tasks at this stage
  (if (< (random-float) new/old-ratio)
    :new :random))
;;(let ((n 1000000)) (/ (loop for i below n if (eql (maths-get-task-method) :new) count 1) (float n)))

(cl-defun maths-current-task ()
  "Use this method to retrive a copy of current task"
  (unless *maths-current-task*
    (setf *maths-current-task* (maths-get-task :method :next)))
  (copy-tree *maths-current-task*))
;;(maths-current-task)

(cl-defun maths-get-task (&key (method :next) (rating +maths-default-rating+))
  "Method is one of :new (default), :random, :current and :last"
  (case method
    (:last (maths-db-last-task))
    (:new (let ((task (maths-create-task :rating rating)))
	    (if (cl-notany #'null task)
	      (maths-db-insert-task task)
	      (maths-db-random-task :rating rating))))
    (:random (maths-db-random-task :rating rating))
    (:current (maths-current-task))
    (:next (maths-get-task :method (maths-get-task-method) :rating rating))
    (:update-current (maths-db-get-task (maths-task-id (maths-current-task))))))

(cl-defun maths-get-task-weird (&key (method :next) (rating +maths-default-rating+))
  "Method is one of :new (default), :random, :current and :last"
  (maths-db-insert-task (maths-create-task :rating rating)))
;;(maths-get-task :method :new)
;;(maths-get-task :method :random :rating 1500)

(cl-defun maths-draw-task (&key (method :next) (rating +maths-default-rating+))
  (setf *maths-current-task* (maths-get-task :method method :rating rating)))

(cl-defun maths-task-ratings (&optional (task (maths-current-task :last)))
  (list (maths-task-rating task) (maths-task-RD task)))


;;; other
(cl-defun maths-current-ratings (&optional (user (maths-current-user t)) (task (maths-get-task :method :current)))
  (list (maths-user-ratings user) (maths-task-ratings task)))
;;(maths-get-task :method :current)

(defun maths-operators ()
  '(:addition :substraction :multiplication :division))
;;(maths-operators)

(defun maths-random-operation ()
  (first (elt-random *maths-task-range*)))
;;(maths-random-operation)

(defun maths-random-level (operation)
  (elt-random (apply #'a-b (second (assoc operation *maths-task-range*)))))
;;(maths-random-level :substraction)

(defun maths-arguments-1 (level)
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

(defun maths-arguments (level operation)
  "Returns "
  (if (eql operation :division)
    (loop for args = (maths-arguments-1 level)
	  while (zerop (first args))
	  finally (return (list (apply #'* args) (first args))))
    (maths-arguments-1 level)))
;;(loop for i below 10000 never (= (second (maths-arguments 1 :division)) 0))

(defun maths-estimate-level (task)
  (destructuring-bind (x y) (maths-task-arguments task)
    (cond
      ;; both < 10
      ((and (< x 10) (< y 10))
       ;;... then either level 1 or 2
       (if (<= (+ x y) 10) 1 2))
      ((and (< x 100) (< y 100)) 3)
      ((and (< x 1000) (< y 1000)) 4))))
;;(mapcar #'maths-estimate-level (maths-db-tasks))

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

(defun maths-sort-task-predicate ()
  "Not in use. However an example on use of `combine<'"
  (combine< (list #'(lambda (x y) (l-explicit< x y (maths-operators))) #'maths-task-operation)
	    (list #'< #'maths-task-level)))
;;(sort (maths-db-tasks) (maths-sort-task-predicate))
;;(ld-select :tasks :columns (:operation :level))

(defun maths-group-tasks-by-type (task)
  "Not in use, but a good example on advanced use of
  #'maths-sort-task-predicate"
  (let ((tasks (maths-db-tasks))
	(pred (maths-sort-task-predicate)))
    (group (sort tasks pred) :test (lt->equal pred))))

(defun maths-estimate-task-rating (operation level)
  "Direct select, violates policy on "
  (aif (maths-db-ratings-by-type operation level)
    (average it)
    +maths-default-rating+))
;;(maths-estimate-task-rating :substraction 2)

(defun maths-init-rating (&optional operation level)
  +maths-default-rating+)

(defun maths-calculate (operation args)
  (apply (maths-operator operation) args))

(defun maths-operator (operation)
  (case operation
    (:addition #'+)
    (:substraction #'-)
    (:multiplication #'*)
    (:division #'/)))
;;(mapcar #'maths-operator '(:addition :substraction :multiplication :division))

(defun task-exists (operation args)
  (ld-select :tasks
    :where (and (eql :operation operation)
		(equal :arguments args))))
;;(task-exists :addition '(0 0))

;;(cl-defun maths-create-task (&key (operation :multiplication) (level 2) (rating +maths-default-rating+))
(cl-defun maths-create-task (&key (operation (maths-random-operation)) (level (maths-random-level operation)) (rating +maths-default-rating+))
  "Creates a new task (sans ID and metadata). It makes sure that
the estimated rating is within the default window around RATING.
Implementaion is kind of overkill, but makes sure that the number
of tries is quite limited"
  (setf rating +maths-default-rating+)
  (cl-flet ((task*-rating (task*) ;; since ID is lacking it is not #'sixth but
	      (or (fifth task*)
		  (error "qwe")))
	    (gentask (level &optional (RD +maths-default-RD+))
	      (loop for i below 1000
		    for args = (maths-arguments level operation)
		    unless (task-exists operation args)
		    return (list operation level args
				 (maths-calculate operation args)
				 (maths-estimate-task-rating operation level)
				 350))))
    (let ((min-rating (- rating +maths-default-rating-window+))
	  (max-rating (+ rating +maths-default-rating-window+))
	  (task* (gentask level))
	  (level-range (second (assoc operation *maths-task-range*))))
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
;;(loop repeat 1 collect (maths-create-task :rating 1771.5996682195228 :operation :addition))
;;(loop repeat 1 collect (maths-create-task :rating 1500))
;;(count nil (loop repeat 100 collect (maths-create-task :rating 1653.5199667679983)))

(cl-defun maths-score (task answer time-elapsed &optional (free-time 2000) (max-time 20000))
  "All TIMEs are in milliseconds."
  (let ((solution (maths-task-solution task)))
    (if (nequal answer solution)
      0
      (- 1 (min 1 (/ (max 0 (- time-elapsed free-time))
		     (coerce (- max-time free-time) 'float)))))))
;;(mapcar #'(lambda (x) (maths-score (first *maths-current-task*) (task-result (first *maths-current-task*)) x)) (a-b 0 22000 2000))

(defsubst maths-invert-score (score) (- 1 score))

;;; Ratings
(defun glicko-new-ratings (user task score &optional time)
  (let* ((uratings (maths-user-ratings user))
	 (tratings (maths-task-ratings task))
	 (res (list (glicko-rating uratings (list tratings score) time)
		    (glicko-rating tratings (list uratings (maths-invert-score score)) time))))
    (message "Calculating ratings %S ==> %S" (list uratings tratings) res)
    res))
;;(glicko-rating '(1372 350) '((1677 35) 0.04) nil)

(defun extract-ratings (user task)
  (list (list (maths-user-rating user)
	      (maths-user-RD user))
	(list (maths-task-rating task)
	      (maths-task-RD task))))

(defun maths-report-answer-strange-error (answer time-elapsed)
  (let* ((user (maths-current-user))
	 (task (maths-current-task))
	 (score (maths-score task answer time-elapsed))
	 (old-ratings (extract-ratings user task ))
	 (new-ratings (glicko-new-ratings user task score)))
    (assert (not (equal old-ratings new-ratings)))
    (destructuring-bind (updated-user updated-task)
	(apply #'maths-db-report-match
	       user task
	       (iso-date-and-time) answer time-elapsed
	       new-ratings)
      ;; Update current user and task
      ;; For some reason these updates lead to an error
      (maths-set-current-user updated-user)
      (maths-set-current-task updated-task))
    score))

(defun maths-report-answer (answer time-elapsed)
  (let* ((user (maths-current-user))
	 (task (maths-current-task))
	 (score (maths-score task answer time-elapsed))
	 (old-ratings (extract-ratings user task))
	 (new-ratings (glicko-new-ratings user task score)))
    (assert (not (equal old-ratings new-ratings)))
    (destructuring-bind (updated-user updated-task)
	(apply #'maths-db-report-match
	       user task
	       (iso-date-and-time) answer time-elapsed
	       new-ratings)
      (list updated-user updated-task))
    score))

(defun maths-init ()
  (maths-db-init))

(defun maths-reset-all ()
  (when (yes-or-no-p "Are you sure you want to reset everything? ")
    (maths-init-database)
    (nilf *maths-current-user* *maths-current-task*)))
;;(maths-reset-all)

(defun maths-save ()
  (ld-save-database *current-database*))


;;; reports
(cl-defun maths-top-ratings (entity &key (from 0) (to 50))
  "ENTITY is either :users or :tasks"
  (maths-db-ratings entity :from from :to to))
;;(maths-top-ratings :tasks)

(provide 'maths-backend)
