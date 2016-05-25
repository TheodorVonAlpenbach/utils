;;;; Labrador Api
;;;; Prototype for reading time filter strings
;;;; For implementation in PHP, check out
;;;; http://lucene.apache.org/solr/4_1_0/solr-core/org/apache/solr/util/DateMathParser.html

(require 'mb-utils-time)

;;;; Q is the queue variable it contains one string
(lexical-let ((q nil))
  (defun q-test (&optional reset)
    (when (or reset (not q))
      (setq q (list reset ())))
    q))
;;(progn (setf (first (q-test 1)) "qkjdf lksj"))
;;(q-test)

(defalias 'q-stream 'first)
(defalias 'q-stack 'second)

(defun q-eat (q position)
  "Removes the first POSITION characters from Q"
  (let ((res (substring (q-stream q) 0 position)))
    (push res (q-stack q))
    (setf (q-stream q) (substring (q-stream q) position))
    res))

(defun q-eat-regexp (q regexp)
  "Checks if beginning of Q matches REGEXP. If so, this part of Q
is `q-eat'-en"
  (awhen (string-match* (format "^%s" regexp) (q-stream q))
    (q-eat q (length it))))
;;(q-eat-regexp (q-test 1) *iso-date*)

(defun* q-barf (q &optional (n 1))
  "Reverts the last N operations on Q by `q-eat'
Seems to be obsolete now."
  (loop repeat (min n (length (q-stack q)))
	(setf (q-stream q) (concat (q-stream q) (pop (q-stack q))))))

(defmacro q-try (q &rest body)
  "Executes BODY `q-eat'-ing up q stream according to expressions
in BODY. If BODY evaluates to nil, the calls to q-eat are
cancelled"
  (let ((gorig-stream (gensym))
	(gres (gensym)))
    `(let* ((,gorig-stream (q-stream ,q))
	    (,gres (progn ,@body)))
       (or ,gres
	   (progn (setf (q-stream ,q) ,gorig-stream)
		  nil)))))
(def-edebug-spec q-try t)
;;;;

(defun lab-dttm (time &optional with-seconds)
  "Local shortcut for `iso-date-and-time'"
  (iso-date-and-time :time time :with-seconds with-seconds))

(defconst *lab-rolling-tokens* 
  '((now "now")
    (daystart "daystart")
    (weekstart "weekstart")
    (monthstart "monthstart")
    (yearstart "yearstart")) )
(defun lab-rolling-token (symbol)
  "Returns the rolling time entities regexp corresponding to SYMBOL.
See `*lab-rolling-tokens*"
  (second (assoc symbol *lab-rolling-tokens*)))
;;(lab-rolling-token 'daysstart)

(defconst *lab-operator-tokens* 
  '((plus "+")
    (minus "-")
    (asterisk "*")
    (open-left "(")
    (open-right ")")
    (closed-left "\\[")
    (closed-right "\\]")))
(defun lab-operator-token (symbol)
  "Returns the operator regexp corresponding to SYMBOL.
See `*lab-operator-tokens*"
  (second (assoc symbol *lab-operator-tokens*)))

(defconst *lab-unit-tokens* 
  `((seconds ,(regexp-opt '("seconds" "s") 'words))
    (minutes ,(regexp-opt '("minutes" "min") 'words))
    (hours ,(regexp-opt '("hours" "h") 'words))
    (days ,(regexp-opt '("days" "d") 'words))
    (weeks ,(regexp-opt '("weeks" "w") 'words))
    (months ,(regexp-opt '("months" "m") 'words))
    (years ,(regexp-opt '("years" "y") 'words))))

(defun lab-unit-token (symbol)
  "Returns the unit regexp corresponding to SYMBOL.
See `*lab-unit-tokens*"
  (second (assoc symbol *lab-unit-tokens*)))
;;(lab-unit-token 'seconds)

(defun lab-period-parse (q)
  "Not implemented. The purpose is to parse higher
level (general) time sets, built on atomic ones, which are
presented below. Beyond scope of current project"
  (lab-time-point-parse q))
;;(progn (lab-period-parse (q-test 1)) (q-test))

(defun lab-time-point-parse (q)
  "Parses expressions of time points. See `decode-time' for the
return value format"
  (q-try q
    (apply #'add-time* 
	   (lab-time-point-atom-parse q)
	   (lab-time-point-modifiers q))))
;;(lab-time-point-parse (q-test "2013-04-11+1days-2weeks"))

(defun lab-time-point-atom-parse (q)
  "Parses expressions of time point atoms. See `decode-time' for
the return value format"
  (or (lab-time-point-atom-fixed-parse q)
      (lab-time-point-atom-rolling-parse q)))
;;(mapcar #'lab-time-point-atom-parse '("2013-04-11" "now"))

(defun lab-dttm-parse (q)
  ""
  (let ((res (or (q-eat-regexp q *iso-dttm*)
		 (q-eat-regexp q *iso-time*)
		 (q-eat-regexp q *iso-date*))))
    (when res (decode-iso-date res))))

(defun lab-time-point-atom-fixed-parse (q)
  "Parses fixed time point atoms, i.e. ISO date and time
strings. See `decode-time' for the return value format"
  (lab-dttm-parse q))
;;(mapcar #'lab-time-point-atom-fixed-parse '("2013-04-11" "now"))

(defun lab-now-parse (q)
  (and (q-eat-regexp q (lab-rolling-token 'now))
       (now)))
(defun lab-daystart-parse (q)
  (and (q-eat-regexp q (lab-rolling-token 'daystart))
       (midnight)))
(defun lab-weekstart-parse (q)
  (and (q-eat-regexp q (lab-rolling-token 'weekstart))
       (weekstart)))
(defun lab-monthstart-parse (q)
  (and (q-eat-regexp q (lab-rolling-token 'monthstart))
       (monthstart)))
(defun lab-yearstart-parse (q)
  (and (q-eat-regexp q (lab-rolling-token 'yearstart))
       (yearstart)))

(defun lab-time-point-atom-rolling-parse (q)
  ""
  (or (lab-now-parse q)
      (lab-daystart-parse q)
      (lab-weekstart-parse q)
      (lab-monthstart-parse q)
      (lab-yearstart-parse q)))

;;modifiers
(defun lab-time-point-modifiers (q)
  (let ((res (loop for m = (lab-time-point-modifier q) 
		   while m collect m)))
    (when res
      (apply #'mapcar* #'+ res))))

(defun lab-time-point-modifier (q)
  (q-try q
    (let ((operator (lab-operator-parse q))
	  (quantity (lab-number-parse q))
	  (unit (lab-time-unit-parse q)))
      (when (and operator quantity unit) 
	(let ((res (make-list 7 0))
	      (q (string-to-number quantity)))
	  (setf (nth (position (iintern unit) '(years months weeks days hours minutes seconds))
		     res)
		(if (equal operator "+") q (- q)))
	  res)))))

;;quantity
(defun lab-number-parse (q)
  "Only simple integers so far"
  (q-eat-regexp q "[0-9]+"))

;;modifier operators
(defun lab-operator-parse (q)
  (or (q-eat-regexp q (lab-operator-token 'plus))
      (q-eat-regexp q (lab-operator-token 'minus))))

;;modifier units
(defun lab-time-unit-parse (q)
  (or (q-eat-regexp q (lab-unit-token 'seconds))
      (q-eat-regexp q (lab-unit-token 'minutes))
      (q-eat-regexp q (lab-unit-token 'hours))
      (q-eat-regexp q (lab-unit-token 'days))
      (q-eat-regexp q (lab-unit-token 'weeks))
      (q-eat-regexp q (lab-unit-token 'months))
      (q-eat-regexp q (lab-unit-token 'years))))

;; INTERVAL
(defun lab-interval-argument-parse (q leftp)
  (or (lab-time-point-parse q)
      (and (q-eat-regexp q (lab-operator-token 'asterisk))
	   (if leftp (the-creation) (the-apocalypse)))))

(defun lab-interval-parenthesis-parse (q leftp)
  (if leftp
    (or (and (q-eat-regexp q (lab-operator-token 'open-left)) 'open-left)
	(and (q-eat-regexp q (lab-operator-token 'closed-left)) 'closed-left))
    (or (and (q-eat-regexp q (lab-operator-token 'open-right)) 'open-right)
	(and (q-eat-regexp q (lab-operator-token 'closed-right)) 'closed-right))))

(defun lab-interval-parse (q)
  "Returns an interval"
  (q-try q
    (let ((iv (lab-interval-atom-parse q))
	  (mod (lab-time-point-modifiers q)))
      (when iv
	(setf (second iv) (apply #'add-time* (second iv) mod))
	(setf (third iv) (apply #'add-time* (third iv) mod)))
      iv)))
;;(lab-interval-parse (q-test "[now now]+1day"))

(defun lab-interval-atom-parse (q)
  "Returns an interval"
  (or 
   (q-try q
    (let ((left-parenthesis (lab-interval-parenthesis-parse q t))
	  (from (lab-interval-argument-parse q t))
	  (space (q-eat-regexp q " *"))
	  (to (lab-interval-argument-parse q nil))
	  (right-parenthesis (lab-interval-parenthesis-parse q nil)))
      (when (and left-parenthesis from to right-parenthesis)
	(list left-parenthesis from to right-parenthesis))))))

(defun lab-convert-delimiters (delimiter)
  (case delimiter
    (open-left "<") 
    (closed-left "<=")
    (open-right ">")
    (closed-right ">=")))
;;(mapcar #'lab-convert-delimiters '(open-left open-right closed-right closed-left))

(defun lab-time-string-to-sql (lp lv rv rp)
  (format "((%s %s x) AND (x %s %s))"
    (lab-dttm lv)
    (lab-convert-delimiters lp)
    (lab-convert-delimiters rp)
    (lab-dttm rv)))

(defun solr-time (time &optional open)
  "Assumes the time resolution is in seconds"
  (if (find time (list (the-creation)(the-apocalypse)) :test #'equal)
    "*"
    (if open 
      (format "%s.999" (lab-dttm (add-time time :second -1) t))
      (lab-dttm time t))))
;;(mapcar (bind #'solr-time t) (list (the-creation) (the-apocalypse) (noon)))

(defun lab-time-string-to-solr (lp lv rv rp)
  (format "[%s TO %s]"
    (solr-time lv (eq lp 'open-left))
    (solr-time rv (eq rp 'open-right))))

(defun* lab-time-string (string &optional (format 'sql))
  (multiple-value-bind (lp lv rv rp)
      (lab-interval-parse (q-test string))
    (if (empty-string-p (q-stream (q-test)))
      (case format
	(solr (lab-time-string-to-solr lp lv rv rp))
	(t (lab-time-string-to-sql lp lv rv rp)))
      (message "Couldn't parse rest of stream: %s" (first (q-test))))))
;;(lab-time-string "(now-1years now)+2days-1weeks+5hours" 'sql)
;;(lab-time-string "(now-1years now)+2days-1weeks+5hours" 'solr)

(provide 'lab-api-time-parser)
