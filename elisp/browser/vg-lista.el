(require 'wget)
(require 'mb-utils-xml)

(defconst +vg-url-prefix+ "http://lista.vg.no/liste/topp-20-single/1/dato"
  "Prefix of VG URL. The complete URL are given by the function
  `vg-url'. An URL typically looks like
http://lista.vg.no/liste/topp-20-single/1/dato")

(defconst +vg-first-week+ '(1958 42)
  "Prefix of VG URL. The complete URL are given by the function
  `vg-url'. An URL typically looks like
http://lista.vg.no/liste/topp-20-single/1/dato")

(defconst +vg-db-directory+ "~/data/vg-listen"
  "No doc")

(defun vg-path (decade)
  (expand-file-name (format "vg-listene-%02d.eldata" decade) +vg-db-directory+))
;;(mapcar #'vg-path '(90 0 10))

(defun vg-some-date-first-vg-week ()
  "Returns first VG week as a date (within that week).
Note that we are using that fact that January 4 is _always_ in week 1."
  (add-time '1958-01-04 :week 41))
;;(week-number (vg-some-date-first-vg-week)) ==> 42

(defun vg-week-exists-p (year week)
  (or (> year (first +vg-first-week+))
      (and (= year (first +vg-first-week+))
	   (>= week (second +vg-first-week+)))))

(defun vg-url (year week)
  "Returns the URL to VG-lista for WEEK in YEAR."
  (assert (vg-week-exists-p year week) t "VG-lista wasn't even started in this week!")
  (format "%s/%d/uke/%02d" +vg-url-prefix+ year week))
;;(vg-url 2011 1)

(defun vg-download-list-html (year week)
  "Returns the html content of URL"
  (wget-to-string (vg-url year week)))
;;(vg-download-list-html 2011 1)

(defun vg-extract-table (html)
  (first (xml-extract-nodes html "table" '(("class" "chart")))))

(defun vg-extract-rows (html)
  (xml-extract-nodes (vg-extract-table html) "tr" '(("onclick" "*"))))

(defun vg-parse-row (html)
  (let* ((place (string-to-number (xml-inner-text (first (xml-extract-nodes html "th" '(("scope" "row")))))))
	 (tdleft (first (xml-extract-nodes html "td" '(("class" "left")))))
	 (2anchors (xml-extract-nodes tdleft "a"))
	 (title (xml-inner-text (first 2anchors)))
	 (artist (xml-inner-text (second 2anchors)))
	 (previous-place (string-to-number (xml-inner-text (first (xml-extract-nodes html "td"))))))
    (list place artist title previous-place)))
;;(vg-update-db '2014-04-01 '2014-04-03 nil)

(defun vg-parse-list (html)
  "Downloads and parses a list"
  (loop for row in (vg-extract-rows html)
	collect (vg-parse-row row)))

(defun vg-download-list (year week)
  "Downloads and parses an list entry"
  (list 'vg-lista :year year :week week :list (vg-parse-list (vg-download-list-html year week))))

(cl-defmacro do-vg-weeks ((year week from &optional to) &rest body)
  "FROM and TO are time designators. YEAR and WEEK are symbols
for use in BODY."
  `(loop with end-date = (parse-time (or ,to (now)))
	for date = (parse-time ,from) then (add-time date :week 1)
	for ,year = (week-year date)
	for ,week = (week-number date)
	while (time< date end-date)
	do (progn ,@body)))
;;(do-vg-weeks (year week '2013-03-15) (message "year = %d and week-number = %d" year week))

(defun vg-add-db-entry (entry db)
  "Updates database DB with all lists in time span FROM TO."
  (insert "'")
  (prin1 entry db)
  (newline 2))

;;; Important note! the charts contains errors for certain weeks. For
;;; instance dato/1991/uke/51 does exist, but not its successors
;;; dato/1991/uke/52 and dato/1992/uke/1 (but dato/1992/uke/2 does
;;; exists)

;;; A reason for this might be that the VG chart is printed in
;;; Wednesday issues. And in the two weeks with missing links,
;;; Wednesday falls on holidays (Christmas day and 1st of January,
;;; respectively).

;;; Anyway, whatever the reason for a missing week, these cases have
;;; to be handled without confusing results. The main problem now is
;;; that for "missing weeks" the VG chart server does not report that
;;; the week is missing or no data is available. It just returns the
;;; data for another week! In some cases it returns the data for the
;;; last week containing true data. In other cases it returns the list
;;; for the current week.

;;; Proposed solution. Process as normal. Then go through the whole
;;; list and delete the weeks that do not seem to be the week
;;; following the previous (correct) one. This comparison could be by
;;; checking that the place number for the prevoius week entry equals
;;; the last-week-place number for the current week entry. Also, if there is
;;; a mismatch, report it in addition to deleting it.
(cl-defun vg-update-db (from &optional (to (now)) (pathname +vg-db-directory+))
  "Updates database DB with all lists in time span FROM TO.
TODO! See above."
  (let* ((n (floor (time- to from :unit :week)))
	 (i 0)
	 (progress-reporter (make-progress-reporter "Collecting VG-lista..." i (+ 1 i n))))
    (with-temp-file pathname
      (do-vg-weeks (year week from to)
	(progress-reporter-update progress-reporter (incf i))
	(vg-add-db-entry (vg-download-list year week) (current-buffer)))
      (progress-reporter-done progress-reporter))))
;;(vg-update-db (now :month -1) (now) "~/data/vg-listen/test.eldata")

(defun vg-update-all ()
  (vg-update-db (vg-some-date-first-vg-week) '1969-12-31 (vg-path 60))
  (vg-update-db '1970-01-04 '1979-12-31 (vg-path 70))
  (vg-update-db '1980-01-04 '1989-12-31 (vg-path 80))
  (vg-update-db '1990-01-04 '1999-12-31 (vg-path 90))
  (vg-update-db '2000-01-04 '2009-12-31 (vg-path 00))
  (vg-update-db '2010-01-04 (vg-path 10)))
;;(vg-update-all)

(defun vg-valid-week-p-old (curr prev)
  "Obsolete. See new version below."
  (loop for (place artist song place-prev) in (seventh curr)
	never (and (typep place-prev '(integer 1 10))
		   (destructuring-bind (prev-place prev-artist prev-song &rest args)
		       (elt (seventh prev) (1- place-prev))
		     (when (or (string/= artist prev-artist)
			       (string/= song prev-song))
		       (message "Entries %S and %S do not match!"
				(list artist song place-prev)
				(list prev-place prev-artist prev-song)))))))

(cl-defun list-similarity (x y &rest args)
  (/ (float (length (apply #'cl-intersection x y args)))
     (float (length (apply #'cl-union x y args)))))
;;(list-similarity '(1 2 3) '(1 7))

(defun vg-song-list-similarity (x y)
  (list-similarity x y :key #'(lambda (pair) (apply #'concat pair)) :test #'string=))

(cl-defun vg-valid-week-p (curr prev &optional (limit 0.3))
  (cl-flet ((song/artist (x) (subseq x 1 3)))
    (let ((x (mapcar #'song/artist (seventh curr)))
	  (y (mapcar #'song/artist (seventh prev))))
      (nor (equal x y)
	   (< (vg-song-list-similarity x y) 0.3)))))
;;(vg-valid-week-p xcurr xprev)
;;(vg-valid-week-p (third qwe) (second qwe))
;;(vg-read-file-to-hashtable (vg-path 0))

(defun vg-add-entry-to-ht (entry hashtable)
  "Adds week ENTRY to HASHTABLE.
ENTRY has the form '(VG-LISTA :YEAR YEAR :WEEK WEEK :LIST ((1
ARTIST1 SONG-TITLE1 PREV-PLACE1) (2 ARTIST2 SONG-TITLE2
PREV-PLACE2)...)). The PREV-PLACEs are integers and are the place
of the song in the previous week entry."
  (loop with year = (third entry)
	with week = (fifth entry)
	for song-parameters in (seventh entry)
	for place = (first song-parameters)
	for key = (subseq song-parameters 1 3)
	for value = (list place year week)
	do (aif (gethash key ht)
	     (push value (gethash key ht))
	     (puthash key (list value) ht))))

(defconst +vg-ok-weeks+
  '((2005 1))
  "These weeks have been detected as spurious by
  #'vg-valid-week-p, but should still be considered ok. For
  instance, in the case '(2005 1) there is a probable
  'mispelling' in the vg html")

(defconst +vg-spurious-weeks+
  '((:year 2010 :week 46 "Doesn't exist")
    (:year 2010 :week 48 "Doesn't exist")))

(defun vg-manually-validated-p (entry)
  (let ((year (getf (rest entry) :year))
	(week (getf (rest entry) :week)))
    (find (list year week) +vg-ok-weeks+ :test #'equal)))
;;(mapcar #'vg-manually-validated-p qwe)

(cl-defun vg-read-file-to-hashtable-1 (pathname ht)
  (with-temp-buffer
    (insert-file-contents pathname)
    (goto-char (point-min))
    (let ((entry (second (read (current-buffer))))
	  prev-entry)
      (while (setf prev-entry entry
		   entry (second (read (current-buffer))))
	(if (or (vg-manually-validated-p entry)
		(vg-valid-week-p entry prev-entry))
	  (vg-add-entry-to-ht entry ht)
	  (message "Entry %S was ommitted!" (subseq entry 1 5))))
      ht)))

(cl-defun vg-read-file-to-hashtable (&optional (pathname +vg-db-directory+) (ht (make-hash-table :test #'equal)))
  (condition-case nil
      (vg-read-file-to-hashtable-1 pathname ht)
    (error nil)))
;;(vg-read-file-to-hashtable (vg-path 10))

(cl-defun vg-read-all-files-to-hashtable (&optional (ht (make-hash-table :test #'equal)))
  (loop for decade in '(60 70 80 90 00 10)
	do (vg-read-file-to-hashtable (vg-path decade) ht))
  ht)
;;(vg-read-all-files-to-hashtable)

(defun vg-place-score (appearances &optional max-place-score)
  (let ((absolute-score (reduce #'+ (mapcar (compose (bind #'- 21 1) #'first) appearances))))
    (if max-place-score 
      (/ (coerce absolute-score 'float) max-place-score)
      absolute-score)))
;;(vg-place-score '((10 1958 49) (10 1958 48) (7 1958 47) (3 1958 46) (2 1958 45) (1 1958 44) (1 1958 43) (1 1958 42)) 1007)
;;(vg-place-score (second (find '("Nazareth" "Love Hurts") *vg-vector* :key #'car :test #'equal)))

(defun vg-duration-score (appearances &optional max-duration-score)
  (let ((absolute-score (length appearances)))
    (if max-duration-score
      (/ (coerce absolute-score 'float) max-duration-score)
      absolute-score)))
;;(vg-duration-score '((10 1958 49) (10 1958 48)) 1007)
;;(vg-duration-score (second (find '("Nazareth" "Love Hurts") *vg-vector* :key #'car :test #'equal)))

(defun max-value (sequence &rest key-args)
  (third (apply #'minimum (coerce sequence 'list) :test #'> key-args)))
;;(max-value (vector '(0 a) '(1 a) '(2 a) '(3 a) '(4 a) '(10 a) '(-30 a)) :key #'first)

(cl-defun vg-max-place-score (&optional (vec *vg-vector*))
  (max-value vec :key (compose #'vg-place-score #'second)))
;;(vg-max-place-score)

(cl-defun vg-max-duration-score (&optional (vec *vg-vector*))
  (max-value vec :key (compose #'vg-duration-score #'second)))
;;(vg-max-duration-score)

(cl-defun vg-score (appearances max-place-score max-duration-score &optional (wlambda 0.8))
  (+ (* wlambda (vg-place-score appearances max-place-score))
     (* (- 1 wlambda) (vg-duration-score appearances max-duration-score))))
;;(vg-score (second (find '("Nazareth" "Love Hurts") *vg-vector* :key #'car :test #'equal)) (vg-max-place-score) (vg-max-duration-score))

(defun vg-list-sorted ()
  (let* ((vec (copy-sequence *vg-vector*))
	 (max-place-score (vg-max-place-score vec))
	 (max-duration-score (vg-max-duration-score vec)))
    (sort* vec #'> :key #'(lambda (x) (vg-score (second x) max-place-score max-duration-score)))))

(defvar *vg-list-sorted* (vg-list-sorted))

(defun vg-weeks (appearances)
  (mapcar #'(lambda (x) (apply #'format "%d-%02d" (rest x))) appearances))

(defun vg-week-span (appearances)
  (let ((weeks (vg-weeks appearances)))
    (list (min-value weeks :test #'string<)
	  (min-value weeks :test #'string>))))
;;(min-value qwe :test #'string>)

(defun vg-week-span-length (appearances)
  (1+ (- (apply #'week- (vg-week-span appearances)))))

(cl-defun vg-list-print-top (n &key stream (list *vg-list-sorted*))
  (cl-loop with max-place-score = (vg-max-place-score) 
	   with max-duration-score = (vg-max-duration-score) 
	   for x being the elements of list
	   for i from 1 to n
	   for appearances = (second x)
	   for score = (vg-score appearances max-place-score max-duration-score)
	   for song = (second (first x))
	   for artist = (first (first x))
	   for duration = (length appearances)
	   for 1sts = (cl-count 1 appearances :key #'first)
	   for weeks = (vg-weeks appearances)
	   for from-week = (min-value weeks :test #'string<)
	   for to-week = (min-value weeks :test #'string>)
	   for weeks-between = (1+ (week- to-week from-week))
	   do (princ (format "%3d. %-40s %-30s %.3f %3d %3d (%2d) %8s %8s\n"
		       i song artist score 1sts duration weeks-between from-week to-week) stream)))
;;(vg-list-print-top 100 :stream (get-buffer "*scratch*"))
;;(vg-list-print-top 100 :stream (get-buffer "*scratch*") :list dsa)
;;(length *vg-list-sorted*)
(setf ewq (mapcar (compose #'vg-week-span-length #'second) *vg-list-sorted*))
(setf ewq (loop for x across *vg-list-sorted*
	     for i from 0 below 5000
	     collect (list i (vg-week-span-length (second x)))))
(time (setf asd (cl-sort ewq #'> :key #'second)))
(setf dsa (loop for i below 100 for (pos weeks) in asd collect (elt *vg-list-sorted* pos)))

(defvar *vg-hashtable* nil)
(defvar *vg-max-place-score* nil "Now, what is this?!")
(defvar *vg-vector* nil)

(defun vg-init-globals ()
  (setf *vg-hashtable* (vg-read-all-files-to-hashtable))
  (setf *vg-max-place-score* (loop for v being the hash-values of *vg-hashtable* maximize (vg-place-score v)))
  (setf *vg-vector* (cl-loop with vec = (make-vector (hash-table-count *vg-hashtable*) nil)
			     for i from 0
			     for k being the hash-keys of *vg-hashtable* using (hash-values v) ;
			     do (setf (aref vec i) (list k v))
			     finally (return vec)))
  (setf *vg-list-sorted* (vg-list-sorted)))
;;(vg-init-globals)

(provide 'vg-lista)
