;;; maths-db-ui.el --- Database access for Maths -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'maths-config)
(require 'maths-db)
(require 'maths-common)
(require 'mb-utils-math)
(require 'mb-utils-sets)
(require 'mb-utils-time)

;;; Users

(defun maths-db-insert-user (name age rating RD)
  "Insert and return a user with NAME, AGE, RATING, and RD."
  (ld-insert :users (list name age rating RD)
             :columns (list :name :age :rating :RD)))

(defun maths-db-get-user (user-designator)
  "Resolve USER-DESIGNATOR to a user row.
Strings select by name, integers by identifier, and rows are
returned unchanged."
  (cond
   ((stringp user-designator)
    (car
     (ld-select :users
       :where (string= user-designator :name))))
   ((integerp user-designator)
    (car
     (ld-select :users
       :where (= user-designator :id))))
   (t user-designator)))

(defun maths-user-id* (user-designator &optional check-validity)
  "Return USER-DESIGNATOR's identifier.
Return an integer designator directly unless CHECK-VALIDITY is
non-nil."
  (if (and (integerp user-designator)
           (not check-validity))
      user-designator
    (when-let ((user (maths-db-get-user user-designator)))
      (maths-user-id user))))

(defun maths-db-last-user ()
  "Return the most recently updated user, or nil."
  (car
   (sort
    (copy-sequence (ld-select :users))
    (lambda (left right)
      (string>
       (maths-user-last-updated left)
       (maths-user-last-updated right))))))

(cl-defun maths-db-user-names (&key sans)
  "Return user names except those in SANS."
  (let ((excluded
         (cond
          ((null sans) nil)
          ((listp sans) sans)
          (t (list sans)))))
    (ld-select :users
      :where (not (cl-member :name excluded :test #'string=))
      :column :name)))

(defun maths-db-user-ratings (user-designator)
  "Return rating history entries for USER-DESIGNATOR."
  (ld-select :matches
    :where (= :user-id (maths-user-id* user-designator))
    :columns (:user-rating ::created)))

;;; Tasks

(defun maths-db-last-task ()
  "Return the first task row, which is the latest inserted row."
  (car (ld-table-data* :tasks)))

(cl-defun maths-db-random-task
    (&key
     (rating +maths-default-rating+)
     (window +maths-default-rating-window+)
     (idle-minutes 10))
  "Return a random task near RATING that has been idle long enough.
Expand WINDOW until a task is found.  Return nil rather than
recur indefinitely when no tasks exist."
  (let ((tasks
         (ld-select :tasks
           :where
           (and
            (and :rating
                 (within :rating (1-sphere window rating)))
            (time< ::updated
                   (add-time (now) :minute (- idle-minutes)))))))
    (cond
     (tasks
      (elt-random tasks))
     ((< window 100000)
      (maths-db-random-task
       :rating rating
       :window (* 1.5 window)
       :idle-minutes idle-minutes))
     ((not (zerop idle-minutes))
      (maths-db-random-task
       :rating rating
       :window window
       :idle-minutes 0))
     (t nil))))

(defun maths-db-insert-task (task-arguments)
  "Insert TASK-ARGUMENTS, excluding identifier and metadata."
  (ld-insert :tasks task-arguments))

(defun maths-db-get-task (identifier)
  "Return the task with IDENTIFIER, or nil."
  (car
   (ld-select :tasks
     :where (= :id identifier))))

(defun maths-db-tasks ()
  "Return all task rows."
  (ld-select :tasks))

(defun maths-db-ratings-by-type (operation level)
  "Return ratings for tasks matching OPERATION and LEVEL."
  (ld-select :tasks
    :where (and (eq :operation operation)
                (= :level level))
    :column :rating))

;;; Matches

(defun maths-db-report-match
    (user task iso-time answer time
          new-user-rating-RD new-task-rating-RD)
  "Insert a match and update USER and TASK ratings.
Return the updated user and task rows."
  (let ((user-id (maths-user-id user))
        (task-id (maths-task-id task)))
    (ld-insert
     :matches
     (list iso-time answer time task-id user-id
           (maths-user-rating user)
           (maths-user-RD user)
           (maths-task-rating task)
           (maths-task-RD task)))
    (list
     (ld-update :users
       (= :id user-id)
       new-user-rating-RD
       (:rating :RD))
     (ld-update :tasks
       (= :id task-id)
       new-task-rating-RD
       (:rating :RD)))))

(provide 'maths-db-ui)

;;; maths-db-ui.el ends here
