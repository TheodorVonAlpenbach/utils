;;;; Important TODO (?)
;;;; Every function calling a ld macro (ld-select, ld-update etc)
;;;; should (probably) be enclosed in an eval-when form
;;;; This is because otherwise the macros are not expanded
;;;; until eval time when the forms already (might) have been

;;;; NB! To load this buffer you must prevent eager macroexpansion. E.g.
;;;; (let ((macroexp--pending-eager-loads '(skip))) (require 'maths-db-ui))

(require 'maths-db)
(require 'maths-common)

;;; Init
(defun maths-db-init () 
  (maths-init-database))

;;; Users
(defun maths-db-insert-user (name age rating RD)
  (ld-insert :users (list name age rating RD)
	     :columns (list :name :age :rating :RD)))
;;(maths-add-user "Ludvik" "7" 1500 350)

(defun maths-db-get-user (user-designator)
  (typecase user-designator
    (string (first (ld-select :users :where (string= user-designator :name))))
    (integer (first (ld-select :users :where (= user-designator :id))))
    (t user-designator)))

(defun maths-user-id* (user-designator &optional check-validity)
  (if (and (integerp user-designator) (not check-validity))
    user-designator
    (maths-user-id (maths-db-get-user user-designator))))
;;(maths-user-id* 5)

(defun maths-db-last-user ()
  (min-element (ld-select :users) :key #'maths-user-last-updated :test #'string>=))
;;(maths-db-last-user)

(cl-defun maths-db-user-names (&key sans)
  (ld-select :users :where (not (member* :name (llist sans) :test #'string=))
	     :column :name))
;;(maths-db-user-names)

(cl-defun maths-db-user-ratings (user-designator)
  (ld-select :matches :where (= :user-id (maths-user-id* user-designator))
	     :columns (:user-rating ::created)))
;;(maths-db-user-ratings "Mats")


;;; Tasks
(defun maths-db-last-task ()
  "Low level function that 'knows' that the first row in a table is the last"
  (first (ld-table-data* :tasks)))
;;(maths-db-last-task)

(cl-defun maths-db-random-task (&key (rating +maths-default-rating+)
				     (window +maths-default-rating-window+)
				     (idle-minutes 10))
  "Low level function that accesses internals of a table.
TODO: Avoid crash on empty db"
  (aif (ld-select :tasks
	 :where (and (awhen :rating (within it (1-sphere window rating)))
		     (time< ::updated (add-time (now) :minute (- idle-minutes)))))
    (random-elt it)
    (if (< window 100000)
      (maths-db-random-task :rating rating :window (* 1.5 window))
      (maths-db-random-task :rating rating :window window :idle-minutes 0))))
;;(maths-db-random-task :rating 3000)

(defun maths-db-insert-task (task-args)
  "task-args is a task without :id (and (:metadata...))"
  (ld-insert :tasks task-args))

(defun maths-db-get-task (id)
  "TODO: this doesn't work when function is loaded, when (:id) is
interpreted as a form to be evaluated later, and which then fails.
Two solutions "
  (first (ld-select :tasks :where (= :id id))))
;;(maths-db-get-task 10)

(defun maths-db-tasks ()
  (ld-select :tasks))
;;(maths-db-tasks)

(defun maths-db-ratings-by-type (operation level)
  "Returns the rating average of all tasks with same :operation and :level as TASK"
  (ld-select :tasks
    :where (and (eql :operation operation) (= :level level))
    :column :rating))
;;(maths-db-ratings-by-type :substraction 2)

;;; Matches
;;; Combined operations
(defun maths-db-report-match (user task iso-time answer time new-user-rating-RD new-task-rating-RD)
  "The core of all updates. It adds a new match into DB, and updates the ratings for both user and task.
It returns the updates of user and task as a pair"
  (destructuring-bind (user-id name age old-user-rating old-user-RD &rest uargs) user
    (destructuring-bind (task-id operation level arguments solution old-task-rating old-task-RD &rest targs) task
      (ld-insert :matches (list iso-time answer time task-id user-id old-user-rating old-user-RD old-task-rating old-task-RD))
      (list (ld-update :users (= (:id) user-id) new-user-rating-RD (:rating :RD))
	    (ld-update :tasks (= (:id) task-id) new-task-rating-RD (:rating :RD))))))

;; Ratings
(cl-defun maths-db-rating-history (user-or-task &key from-time to-time n)
  "Returns a list of user or task ratings in period FROM-TIME TO-TIME"
  (destructuring-bind (iso-times ratings RDs)
      (transpose (maths-get-matches (if (user-p user-or-task) :user :task) user-or-task
				    :columns '(:iso-time :user-rating :user-RD)))
    (transpose (list (push (ld-metadata user-or-task :columns :created) iso-times) 
		     (push-back (ld-column rating user-or-task) ratings)
		     (push-back (ld-column RD user-or-task) RDs)))))

;;(maths-db-ratings :tasks)
;;(ld-select :tasks :columns (:id (round :rating) (round :RD)) :order-by :rating)
;;(ld-select :users :columns (:name (round :rating) (round :RD)) :order-by :rating)

(provide 'maths-db-ui)
;;(require 'maths-db-ui)

