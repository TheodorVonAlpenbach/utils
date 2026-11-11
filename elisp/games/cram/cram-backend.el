;;; cram-backend.el --- Backend for the Cram game -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'cram-config)
(require 'cram-db-ui)
(require 'glicko)
(require 'levenshtein)
(require 'mb-locale)
(require 'mb-utils-iso-time)
(require 'mb-utils-strings)
(require 'mb-utils-time)

;;; Reports

(cl-defun cram-list-user-ratings
    (&optional (buffer-name "*User Ratings*"))
  "Display user ratings in BUFFER-NAME."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ
     (tab-format
      (ld-select :user
        :columns
        (:name
         (round (car :rating))
         (round (cadr :rating))
         ::updated)
        :order-by :rating
        :order :desc)
      :header '("Name" "Rating" "RD" "Last update")
      :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(cl-defun cram-db-last-matches (&optional (count 10))
  "Return display rows for the latest COUNT matches."
  (let ((matches (cram-get-matches)))
    (cl-loop
     for match in
     (cl-subseq matches 0 (min count (length matches)))
     for problem =
     (cram-db-get-problem (cram-match-problem-id match))
     when problem
     collect
     (list
      (cram-match-timestamp match)
      (cram-problem-question problem)
      (cram-match-response match)
      (cram-problem-answer problem)
      (cram-match-score match)))))

(cl-defun cram-db-problem-ratings
    (&optional (filter *cram-ref-filter*))
  "Return problem rating rows whose source identifier matches FILTER."
  (cl-sort
   (if filter
       (ld-select :problem
         :columns
         (:id :question :answer
          (round (car :rating))
          (round (cadr :rating))
          ::updated)
         :where (string-match-p filter :source-id))
     (ld-select :problem
       :columns
       (:id :question :answer
        (round (car :rating))
        (round (cadr :rating))
        ::updated)))
   #'> :key #'cl-fourth))

;;; Current user

(defvar *cram-current-user* nil
  "The current Cram user row.")

(cl-defun cram-add-user
    (name &optional (rating +cram-default-rating+))
  "Insert and return a user with NAME and RATING."
  (cram-db-insert-user name rating))

(defun cram-set-current-user (user)
  "Set and return the current USER.
Resolve string designators through the database."
  (setq *cram-current-user*
        (if (stringp user)
            (cram-db-get-user user)
          user)))

(defun cram-current-user (&optional update)
  "Return the current user.
When UPDATE is non-nil, resolve the most recently updated user
again from the current database."
  (when *current-database*
    (when (or update (null *cram-current-user*))
      (cram-set-current-user
       (or (cram-db-last-user)
           (cram-db-get-user +cram-default-user-name+))))
    *cram-current-user*))

;;; Problem and match selection

(defvar *cram-current-problem* nil
  "The current Cram problem row.")

(defun cram-set-current-problem (problem)
  "Set and return the current PROBLEM."
  (setq *cram-current-problem* problem))

(cl-defun cram-get-problem-method
    (&optional (new-problem-ratio 0.5))
  "Choose between an unused and a history-weighted problem.
Select `:new' with probability NEW-PROBLEM-RATIO."
  (unless (<= 0 new-problem-ratio 1)
    (user-error "Problem ratio must be between 0 and 1"))
  (if (< (cl-random 1.0) new-problem-ratio)
      :new
    :cram))

(defun cram-get-problems ()
  "Return problems matching `*cram-ref-filter*'."
  (cram-db-problems))

(defun cram-get-problem-ids ()
  "Return identifiers of problems matching `*cram-ref-filter*'."
  (mapcar #'cram-problem-id (cram-get-problems)))

(defun cram-get-worst-problem ()
  "Return the lowest-rated problem outside the recent quarantine."
  (let* ((problems
          (cl-sort
           (copy-sequence (cram-get-problems))
           #'string>
           :key #'cram-problem-updated))
         (candidates
          (nthcdr
           (min *cram-same-problem-limit*
                (length problems))
           problems)))
    (or
     (and candidates
          (cl-reduce
           (lambda (left right)
             (if (< (cram-problem-rating-e left)
                    (cram-problem-rating-e right))
                 left
               right))
           candidates))
     (car (last problems)))))

(cl-defun cram-get-matches
    (&optional (user *cram-current-user*) reset)
  "Return cached matches for USER and the active problem filter.
When RESET is non-nil, rebuild the cache from the beginning of
the database history."
  (when reset
    (setq *cram-last-update* (the-creation)
          *cram-match-cache* nil))
  (if (null user)
      nil
    (let* ((problem-ids (cram-get-problem-ids))
           (new-matches
            (ld-select :match
              :where
              (and
               (time< *cram-last-update* ::updated)
               (= :user-id (cram-user-id user))
               (member :problem-id problem-ids)))))
      (setq *cram-last-update* (now)
            *cram-match-cache*
            (append new-matches *cram-match-cache*))
      *cram-match-cache*)))

(defun cram-recent-matches (matches seconds)
  "Return MATCHES recorded within the last SECONDS."
  (cl-remove-if-not
   (lambda (match)
     (< (time- (now)
              (cram-match-timestamp match)
              :second)
        seconds))
   matches))

(defun cram--matches-by-problem (matches)
  "Group MATCHES by problem identifier, preserving input order."
  (let ((groups (make-hash-table :test #'eql))
        identifiers)
    (dolist (match matches)
      (let ((identifier (cram-match-problem-id match)))
        (unless (gethash identifier groups)
          (push identifier identifiers))
        (push match (gethash identifier groups))))
    (mapcar
     (lambda (identifier)
       (nreverse (gethash identifier groups)))
     (nreverse identifiers))))

(cl-defun cram-match-weight
    (score elapsed-seconds
           &optional
           (period (* 2 24 3600.0))
           (retention 0.2))
  "Return a selection weight for SCORE after ELAPSED-SECONDS.
Lower weights indicate a problem that should be reviewed sooner."
  (* score
     (expt retention
           (/ elapsed-seconds period))))

(cl-defun cram-problem-weights
    (&optional (user (cram-current-user)))
  "Return problem identifier/selection-weight pairs for USER."
  (cl-loop
   with current-time = (unix-time)
   for matches in
   (cram--matches-by-problem (cram-get-matches user))
   for scores = (mapcar #'cram-match-score matches)
   for latest = (car matches)
   for successful-streak =
   (or (cl-position-if
        (lambda (score) (<= score 0))
        scores)
       (length scores))
   for elapsed =
   (max
    0
    (- current-time
       (unix-time (cram-match-timestamp latest))))
   collect
   (list
    (cram-match-problem-id latest)
    (cram-match-weight
     (cram-match-score latest)
     (/ elapsed
        (expt 2.0 successful-streak))))))

(cl-defun cram-get-cram-problem-id
    (&optional (user (cram-current-user)))
  "Return the identifier of the problem most due for USER."
  (when-let ((weights (cram-problem-weights user)))
    (car
     (cl-reduce
      (lambda (left right)
        (if (< (cadr left) (cadr right))
            left
          right))
      weights))))

(defun cram-problem (identifier)
  "Return the problem with IDENTIFIER."
  (cram-db-get-problem identifier))

(cl-defun cram-get-cram-problem
    (&optional (user (cram-current-user)))
  "Return an unused problem or the problem most due for USER."
  (or
   (car (cram-db-unused-problems))
   (when-let ((identifier
               (cram-get-cram-problem-id user)))
     (cram-problem identifier))
   (cram-db-random-problem)))

(defun cram-current-problem ()
  "Return a copy of the current problem, selecting one if needed."
  (unless *cram-current-problem*
    (cram-set-current-problem
     (cram-get-problem :method :next)))
  (copy-tree *cram-current-problem*))

(cl-defun cram-get-problem
    (&key
     (method :worst)
     (rating +cram-default-rating+))
  "Return a problem selected according to METHOD.
Supported methods are `:last', `:new', `:random', `:current',
`:next', `:worst', `:cram', and `:update-current'.  RATING is
passed to random selection for compatibility."
  (cl-case method
    (:last (cram-db-last-problem))
    (:new
     (or (car (cram-db-unused-problems))
         (cram-db-random-problem :rating rating)))
    (:random
     (cram-db-random-problem :rating rating))
    (:current
     (copy-tree *cram-current-problem*))
    (:next
     (cram-get-problem
      :method (cram-get-problem-method)
      :rating rating))
    (:worst
     (cram-get-worst-problem))
    (:cram
     (cram-get-cram-problem))
    (:update-current
     (when *cram-current-problem*
       (cram-db-get-problem
        (cram-problem-id *cram-current-problem*))))
    (otherwise
     (user-error "Unknown Cram problem method: %S" method))))

(cl-defun cram-draw-problem
    (&key
     (method :worst)
     (rating +cram-default-rating+))
  "Select and store a problem using METHOD and RATING."
  (cram-set-current-problem
   (cram-get-problem :method method :rating rating)))

(defun cram-current-ratings
    (&optional user problem)
  "Return current USER and PROBLEM rating pairs."
  (let ((current-user (or user (cram-current-user)))
        (current-problem
         (or problem
             *cram-current-problem*
             (cram-current-problem))))
    (when (and current-user current-problem)
      (list
       (cram-user-rating current-user)
       (cram-problem-rating current-problem)))))

;;; Responses

(defun cram--combine-choices (choices)
  "Return the Cartesian product of CHOICES."
  (if (null choices)
      '(nil)
    (cl-loop
     for choice in (car choices)
     append
     (mapcar
      (lambda (tail) (cons choice tail))
      (cram--combine-choices (cdr choices))))))

(defun cram-expand-alternative-pattern (pattern)
  "Expand optional parenthesized parts of PATTERN."
  (let ((choices
         (mapcar
          (lambda (form)
            (let ((rendered (format "%S" form)))
              (if (listp form)
                  (list rendered
                        (substring rendered 1 -1)
                        nil)
                (list rendered))))
          (read-whole-string pattern))))
    (mapcar
     (lambda (parts)
       (string-trim
        (mapconcat
         #'identity
         (delq nil parts)
         " ")))
     (cram--combine-choices choices))))

(defun cram-expand-alternatives (problem)
  "Return every accepted textual answer for PROBLEM."
  (delete-dups
   (mapcan
    #'cram-expand-alternative-pattern
    (cons
     (cram-problem-answer problem)
     (when-let ((alternatives
                 (cram-problem-alternatives problem)))
       (split-string alternatives "|" t "[[:space:]]*"))))))

(defconst +cram-encoding+
  '(("å" "aa" "%E5" 229 "Ã¥")
    ("æ" "ae" "%E6" 230 "Ã¦")
    ("ø" "oe" "%F8" 248 "Ã¸"))
  "Norwegian transliterations accepted in Cram responses.")

(defun cram-normalize-string (string)
  "Normalize STRING for Cram response comparison."
  (iso-latin1-2-7bit
   (downcase (string-trim string))
   +cram-encoding+))

(defun cram-correct-response-p (problem response)
  "Return non-nil when RESPONSE is accepted for PROBLEM.
Optional parenthesized words, configured alternatives, Norwegian
transliterations, and a Levenshtein distance below three are
accepted."
  (cl-member
   (cram-normalize-string response)
   (mapcar
    #'cram-normalize-string
    (cram-expand-alternatives problem))
   :test
   (lambda (left right)
     (< (levenshtein-distance left right) 3))))

(cl-defun cram-score
    (problem response time-elapsed
             &optional
             (free-time 8000)
             (max-time 30000))
  "Score RESPONSE to PROBLEM after TIME-ELAPSED milliseconds."
  (unless (> max-time free-time)
    (user-error "MAX-TIME must be greater than FREE-TIME"))
  (if (cram-correct-response-p problem response)
      (- 1
         (min
          1
          (/ (max 0 (- time-elapsed free-time))
             (float (- max-time free-time)))))
    0))

(defsubst cram-invert-score (score)
  "Return SCORE from the opponent's perspective."
  (- 1 score))

;;; Ratings and persistence

(defun cram-glicko-new-ratings
    (user problem score &optional time)
  "Calculate new USER and PROBLEM ratings from SCORE.
TIME is the optional rating period."
  (let ((user-rating (cram-user-rating user))
        (problem-rating (cram-problem-rating problem)))
    (list
     (glicko-rating
      user-rating
      (list problem-rating score)
      time)
     (glicko-rating
      problem-rating
      (list user-rating (cram-invert-score score))
      time))))

(define-obsolete-function-alias
  'glicko-new-ratings #'cram-glicko-new-ratings "2026-11-11")

(defun cram-extract-ratings (user problem)
  "Return USER and PROBLEM rating pairs."
  (list
   (cram-user-rating user)
   (cram-problem-rating problem)))

(defun cram-report-response (response time-elapsed)
  "Score RESPONSE, persist the match, and update current ratings."
  (let ((user (cram-current-user))
        (problem (cram-current-problem)))
    (unless user
      (user-error "No current Cram user"))
    (unless problem
      (user-error "No current Cram problem"))
    (let* ((score
            (cram-score problem response time-elapsed))
           (new-ratings
            (cram-glicko-new-ratings
             user problem score)))
      (cl-destructuring-bind
          (updated-user updated-problem)
          (apply
           #'cram-db-report-match
           user problem
           (iso-date-and-time :with-seconds t)
           response
           time-elapsed
           score
           new-ratings)
        (cram-set-current-user updated-user)
        (cram-set-current-problem updated-problem))
      score)))

(defun cram-reset-all ()
  "Reset Cram's database and current selections after confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to reset everything? ")
    (cram-init-database t)
    (setq *cram-current-user* nil
          *cram-current-problem* nil
          *cram-match-cache* nil
          *cram-last-update* (the-creation))))

(defun cram-save ()
  "Save the current Cram database."
  (interactive)
  (ld-save-database *current-database*))

(cl-defun cram-backup
    (&optional (backup-type :iso-date))
  "Save a backup of the current Cram database using BACKUP-TYPE."
  (interactive)
  (ld-save-database *current-database* backup-type))

(provide 'cram-backend)

;;; cram-backend.el ends here
