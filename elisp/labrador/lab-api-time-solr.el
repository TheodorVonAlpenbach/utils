;;;; Labrador Period Api
;;;; Prototype for reading time filter strings
;;;; For implementation in PHP, check out
;;;; http://lucene.apache.org/solr/4_1_0/solr-core/org/apache/solr/util/DateMathParser.html

;;;; Logge seg inn på pbl-login01.osl.basefarm.net (>ssh dagen)
;;;; Derfra til pbl-php01.dev (>ssh pbl-php01.dev)

(defun lab-regexp ()
  (let ((space "[[:space:]]*")
	(lp "[[(]")
	(lv "[^[:space:]]+")
	(to "[[:space:]]+")
	(rv "[^])]+")
	(rp "[])]")
	(mod ".*"))
    (format "\\(%s\\)%s\\(%s\\)\\(%s\\)\\(%s\\)\\(%s\\)\\(%s\\)" lp space lv to rv rp mod)))

(defun* lab-dttm (time &optional (with-seconds t))
  "Local shortcut for `iso-date-and-time'"
  (iso-date-and-time :time time :with-seconds with-seconds))

(defun solr-time (time open-p left-p)
  "Assumes the time resolution is in seconds.
Solr does note have a notion of different interval types. Both
interval boundaries are converted to the '< operator. Hence, to
distinguish between (), [], (] and [), we need to hack the
intervals by altering the interval times."
  (if (or (and left-p (equal time (the-creation)))
	  (and (not left-p) (equal time (the-apocalypse))))
    "*"
    (concat (if (eq open-p left-p) 
	      (lab-dttm time t)
	      (format "%s.999" (lab-dttm (add-time time :second -1) t)))
	    "Z")))

(defun lab-time-to-solr-time (s open-p left-p)
  "Converts time string allowed by lab-api to the more strict Solr time string.
lab-api allows parts of time: date only (time part then defaults
to 00:00:00), and time only (date part then defaults to today).
Also, seconds part is not mandatory in lap-api time---it is
indeed in Solr."
  (unless (stringp s) (error "lab-time-to-solr-time: Argument S is nil!"))
  (let ((isopart (string-match* *iso-dttm* s)))
    (if isopart
      (string-replace s isopart (solr-time (decode-iso-dttm isopart) open-p left-p))
      s)))

(defun lab-open-p (p)
  "Returns nil iff P is not an open parenthesis string"
  (or (equal p "(") (equal p ")")))

(defun lab-api-period-to-solr (s)
  "Converts a lab-api time period expression to an equivalent
Solr expression."
  (multiple-value-bind (match lp lv to rv rp mod)
      (string-match* (lab-regexp) s :num '(0 1 2 3 4 5 6))
    (unless (stringp match)
      (error "lab-api-period-to-solr: Argument is not a valid lab interval!"))
    (format "[%s%s TO %s%s]" 
      (lab-time-to-solr-time lv (lab-open-p lp) t) mod
      (lab-time-to-solr-time rv (lab-open-p rp) nil) mod)))
;;(lab-api-period-to-solr "[2013-04-12 16:31]")

(provide 'lab-api-time-solr)
