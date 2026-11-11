;;; cram-db-ui.el --- Database access for Cram -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'cram-config)
(require 'cram-db)
(require 'cram-common)
(require 'mb-lists)
(require 'mb-utils-math)

;;; Users

(defun cram-db-insert-user (name rating)
  "Insert and return a user with NAME and RATING."
  (ld-insert :user (list name rating)
             :columns (list :name :rating)))

(defun cram-db-get-user (user-designator)
  "Resolve USER-DESIGNATOR to a user row.
Strings select by name, integers by identifier, and rows are
returned unchanged."
  (cond
   ((stringp user-designator)
    (car
     (ld-select :user
       :where (string= user-designator :name))))
   ((integerp user-designator)
    (car
     (ld-select :user
       :where (= user-designator :id))))
   (t user-designator)))

(defun cram-user-id* (user-designator &optional check-validity)
  "Return USER-DESIGNATOR's identifier.
Return an integer designator directly unless CHECK-VALIDITY is
non-nil."
  (if (and (integerp user-designator)
           (not check-validity))
      user-designator
    (when-let ((user (cram-db-get-user user-designator)))
      (cram-user-id user))))

(defun cram-db-last-user ()
  "Return the most recently updated user, or nil."
  (car
   (sort
    (copy-sequence (ld-select :user))
    (lambda (left right)
      (string>
       (cram-user-last-updated left)
       (cram-user-last-updated right))))))

(cl-defun cram-db-user-names (&key sans)
  "Return user names except those in SANS."
  (let ((excluded
         (cond
          ((null sans) nil)
          ((listp sans) sans)
          (t (list sans)))))
    (ld-select :user
      :where (not (cl-member :name excluded :test #'string=))
      :column :name)))

(defun cram-db-user-ratings (user-designator)
  "Return rating history entries for USER-DESIGNATOR."
  (ld-select :match
    :where (= :user-id (cram-user-id* user-designator))
    :columns (:user-rating ::created)))

;;; Problems

(defun cram-db-last-problem ()
  "Return the problem from the latest match, or nil."
  (when-let ((match (car (ld-table-data* :match))))
    (cram-db-get-problem (cram-match-problem-id match))))

(defun cram-db-random-problem-1 ()
  "Return a random problem matching `*cram-ref-filter*', or nil."
  (when-let ((problems
              (ld-select :problem
                :where (string-match-p *cram-ref-filter* :source-id))))
    (elt-random problems)))

(cl-defun cram-db-random-problem (&key matches rating)
  "Return a random problem, weighted by MATCHES when supplied.
Problems seen fewer times are weighted more heavily.  RATING is
accepted for compatibility with existing callers but is not yet
part of the selection strategy."
  (ignore rating)
  (if matches
      (let* ((counts
              (accumulate-list
               (mapcar #'cram-match-problem-id matches)))
             (problem-id
              (random-weighted-element counts t)))
        (or (cram-db-get-problem problem-id)
            (cram-db-random-problem-1)))
    (cram-db-random-problem-1)))

(defun cram-db-insert-problem (problem-arguments)
  "Insert PROBLEM-ARGUMENTS, excluding identifier and metadata."
  (ld-insert :problem problem-arguments))

(defun cram-db-get-problem (identifier)
  "Return the problem with IDENTIFIER, or nil."
  (car
   (ld-select :problem
     :where (= :id identifier))))

(defun cram-db-problems (&optional quarantine)
  "Return problems matching `*cram-ref-filter*'.
QUARANTINE is retained for compatibility and is not implemented."
  (ignore quarantine)
  (if *cram-ref-filter*
      (ld-select :problem
        :where (string-match-p *cram-ref-filter* :source-id))
    (ld-select :problem)))

(defun cram-db-unused-problems ()
  "Return problems that do not occur in any match."
  (cl-set-difference
   (cram-db-problems)
   (ld-select :match)
   :test
   (lambda (problem match)
     (= (cram-problem-id problem)
        (cram-match-problem-id match)))))

(defun cram-db-ratings-by-type (operation level)
  "Return ratings for problems matching OPERATION and LEVEL."
  (ld-select :problem
    :where (and (eq :operation operation)
                (= :level level))
    :column :rating))

;;; Matches

(defun cram-db-report-match
    (user problem iso-time response time score
          new-user-rating new-problem-rating)
  "Insert a match and update USER and PROBLEM ratings.
Return the updated user and problem rows."
  (let ((user-id (cram-user-id user))
        (problem-id (cram-problem-id problem)))
    (ld-insert
     :match
     (list iso-time response time score user-id problem-id
           (cram-user-rating user)
           (cram-problem-rating problem)))
    (list
     (ld-update :user
       (= :id user-id)
       (list new-user-rating)
       (:rating))
     (ld-update :problem
       (= :id problem-id)
       (list new-problem-rating)
       (:rating)))))

(provide 'cram-db-ui)

;;; cram-db-ui.el ends here
