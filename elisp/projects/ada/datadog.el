(provide 'datadog)

;; (defconst dd-json (json-parse-string (file-string "~/tmp/apm-response.json")))
;; (defconst dd-json (json-parse-string (file-string "~/ada-tasks/ADA-12003-optimalisere-mongodb-in-operator/data.json")))
;; (defconst dd-json (json-parse-string (file-string "~/ada-tasks/ADA-12003-optimalisere-mongodb-in-operator/data-new.json")))
(defconst dd-json (json-parse-string (file-string "~/ada-tasks/ADA-12003-optimalisere-mongodb-in-operator/new-data.json")))
;; (defconst dd-json (json-parse-string (file-string "~/ada-tasks/ADA-12003-optimalisere-mongodb-in-operator/data-benchmark.json")))

(cl-defun dd-data () (elt (gethash "data" dd-json) 0))
(cl-defun dd-attributes () (gethash "attributes" (dd-data)))
(defconst dd-columns (gethash "columns" (dd-attributes)))
(defconst dd-queries (gethash "values" (elt dd-columns 0)))
(defconst dd-durations (gethash "values" (elt dd-columns 1)))
(defconst dd-hits (gethash "values" (elt dd-columns 2)))

(cl-defun num-in-args-1 (expression)
  (length (cdadr (car expression))))
;;(num-in-args-1 (first (dd-parse-query (elt dd-queries 0))))

(cl-defun num-in-args (expression)
  (cl-destructuring-bind (f s) expression
    (unless (eql (caar f) '_id\.userPseudonym)
      (rotatef f s))
     (cl-assert (eql (caar s) '_id\.componentUuid))
    (mapcar #'num-in-args-1 (list f s))))
;;(num-in-args '(((_id\.userPseudonym ($in . ["?" "?" "?" "?" "?" "?" "?" "?" "?" "?" "?" "?" ...]))) ((_id\.componentUuid ($in . ["?" "?" "?" "?" "?" "?" "?" "?" "?" "?" "?" "?" ...])))))
;;(num-in-args '(((_id (userPseudonym . "?") (componentUuid . "?"))) ((_id (userPseudonym . "?") (componentUuid . "?"))) ((_id (userPseudonym . "?") (componentUuid . "?"))) ((_id (userPseudonym . "?") (componentUuid . "?"))) ((_id (userPseudonym . "?") (componentUuid . "?")))))
;;(num-in-args (dd-parse-query (elt dd-queries 0)))

(cl-defun qwe (mongo-query)
  (awhen (condition-case nil
	     (json-parse-string (elt mongo-query 0) :object-type 'alist)
	   (error nil))
    (when (and (string= (cdr (first it)) "userComponentState")
	       (eql (car (third it)) 'projection)
	       (eql (car (second it)) 'filter))
      (if (eql (caadr (second it)) '$and)
	(cl-coerce (cdr (cadr (second it))) 'list)
	(cl-coerce (cdr (cadr (second it))) 'list)))))
;;(mapcar #'qwe dd-queries)
;;(length (qwe (elt dd-queries 35)))
;;(qwe (elt dd-queries 13))(((_id\.userPseudonym ($in . ["?" "?" "?" "?"]))) ((_id\.componentUuid ($in . ["?"]))))
;;(length dd-queries)

(cl-defun dd-parse-query (mongo-query)
  (awhen (condition-case nil
	     (json-parse-string (elt mongo-query 0) :object-type 'alist)
	   (error nil))
    (when (and (string= (cdr (first it)) "userComponentState")
	       (eql (car (third it)) 'projection)
	       (eql (car (second it)) 'filter))
      (cl-case (caadr (second it))
	($and (num-in-args (cl-coerce (cdr (cadr (second it))) 'list)))
	($or (list 1 (length (cl-coerce (cdr (cadr (second it))) 'list))))
	(_id (list 1 1))
	(t (error "unknown filter"))))))
;;(mapcar #'dd-parse-query dd-queries)
;;(cl-find '(1 1) (mapcar #'dd-parse-query dd-queries) :test #'equal)
;;(mapcar #'dd-parse-query (subseq dd-queries 0 14))
;;(dd-parse-query (elt dd-queries 13))
;;(mapcar #'dd-parse-query (subseq dd-queries 1928 1929))

(defconst dd-table
  (cl-loop for q across dd-queries
	for d across dd-durations
	for h across dd-hits
	for pq = (dd-parse-query q)
	if pq collect (list pq d h)))
;;(length dd-table)
;;(first dd-table)

(cl-defun init-report-table (n m)
  (cons (a-b 0 m)
	(cl-loop for i from 1 to n collect (cons i (make-list m nil)))))

(cl-defun dd-reorganize-table (table)
  (let ((n (min-value table :test #'> :key #'caar))
	(m (min-value table :test #'> :key #'cadar)))
    (let* ((d2 (init-report-table n m))
	   (h2 (copy-tree d2)))
      (cl-loop for ((i j) d h) in table
	    for d-row = (nth i d2)
	    for h-row = (nth i h2)
	    do (setf (nth j d-row) d)
	    do (setf (nth j h-row) h))
      (list d2 h2))))
;;(dd-reorganize-table dd-table)
;;(setf qwe (dd-reorganize-table dd-table))

(defvar csv-durations)
(defvar csv-hits)

(cl-destructuring-bind (d2 h2) (dd-reorganize-table dd-table)
  (setf csv-durations d2)
  (setf csv-hits h2))

;; durations to csv
(write-csv csv-durations "~/ada-tasks/ADA-12003-optimalisere-mongodb-in-operator/apm/apm-durations-new.csv" :overwrite t)
(write-csv csv-hits "~/ada-tasks/ADA-12003-optimalisere-mongodb-in-operator/apm/apm-hits-new.csv" :overwrite t)
;;(length csv-durations)
;;(length csv-hits)

(cl-defun report-sum (list2)
  (cl-loop for r in (rest list2)
	sum (cl-loop for x in (rest r) if x sum x)))

(cl-defun sums ()
  (cl-destructuring-bind (d2 h2) (dd-reorganize-table dd-table)
    (list (report-sum d2) (report-sum h2))))
;;(/ (first (sums)) 24 60 60)

;; (concat* (sort (cl-remove-duplicates (cl-loop for x in dd-table collect (caar x))) #'<) :in " " :key #'sstring)
;; (concat* (sort (cl-remove-duplicates (cl-loop for x in dd-table collect (cadar x))) #'<) :in " " :key #'sstring)
