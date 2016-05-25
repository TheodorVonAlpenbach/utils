(require 'mb-utils-time)

;;; Emacs Lisp implementation of Microsoft variant type DATE. This is
;;; represented as a float where the integer and decimal parts shows
;;; the date and time respectively. The DATE value 0.0 represents
;;; 1899-12-30 0:00. The time part is the fractional elapsed time of
;;; the corresponding date. E.g. DATE value 1.5 is "one and a half
;;; day" since 1899-12-30 0:00, ie. 1899-12-31 12:00.

;;; NB! The MS DATE implementation does not take into consideration
;;; daylight savings. Consequently, daylight savings are ommitted in
;;; this Emacs Lisp implementation as well.

;;; This Emacs Lisp implementation was meant as a prototype for a
;;; Hydro project where a C++/COM implementation receives date from a
;;; VB application. Because of this there was a need for providing
;;; DATE calculations.

;;; Additionally, functions concerning "DATE interval roundings" are
;;; supplied. These are of interest to the above mentioned Hydro
;;; project only. Apart from this the library should useful for other
;;; C++ prototype purposes.

;;; The C++ implementation is found in mb_lib/VC/mb_DATE.h.

;;; Updated 2002-11-25 mb.

(defconst *DATE-start+100* (parse-time "1999-12-30")
  "Since 1900-01-01 is not representable in emacs time, we have to use
this trick by adding 100 years.")
;;(DATE (add-time (parse-time "2003-03-30") :hour 13 :minute 45 :second 11))

(defconst *DATE-first-sunday* 1
  "Least DATE that was a Sunday.")

(defconst *num-days-last-century* (+ (* 365 100) 24))

(defun* DATE (&optional (time (now)))
  "Returns the value of MS type DATE for TIME."
  (+ (time- time *DATE-start+100* :unit :day)
     *num-days-last-century*))
;;(DATE)

(defun DATE-decode (DATE)
  "Transforms MS DATE to an Emacs decoded time. See \(decode-time\)."
  (let* ((days (floor DATE))
	 (secs (round (* (- DATE days) 60 60 24))))
    (add-time *DATE-start+100*
      :day (- days *num-days-last-century*)
      :second secs)))
;;(list (now) (DATE-decode (DATE (now))))

(defun DATE-weekday (DATE)
  "Returns 0 for Sunday, 1 for Monday, ..., 6 for Saturday."
  (mod (floor (- DATE *DATE-first-sunday*)) 7))
;;(loop for day from 0 to 7 for d = (DATE (now :day day)) collect (list d (DATE-weekday d)))

(defun DATE-weekend-p (DATE)
  "Non-nil iff DATE is a Sunday or a Monday."
  (case (DATE-weekday DATE)
    ((0 6) t)
    (otherwise nil)))
;;(loop for day from 0 to 7 collect (DATE-weekend-p (DATE (now :day day))))

(defun DATE-round (from-DATE weekday &optional backward strictly)
  "Returns nearest DATE to FROM-DATE that is WEEKDAY \(see
DATE-weekday\). If option BACKWARD is non-nil the rounding is
performed backwards in time, e.g. 'previous Friday'. Else, rounding is
performed forwards in time. Iff option STRICTLY is non-nil then
FROM-DATE is not itself a valid result."
  (let ((diff (mod (- weekday (DATE-weekday from-DATE)) 7)))
    (if backward
      (if (and (not strictly) (zerop diff))
	(+ from-DATE 0)
	(+ from-DATE diff -7))
      ;;forward
      (if (and strictly (zerop diff))
	(+ from-DATE 7)
	(+ from-DATE diff)))))
;;(DATE-round (floor (DATE)) 3 t nil)
;;(DATE-round (floor (DATE)) 3 nil nil)
;;(DATE-round (floor (DATE)) 3 t t)
;;(DATE-round (floor (DATE)) 3 nil t)

(defun DATE-interval (DATE1 DATE2) (list DATE1 DATE2))

(defun DATE-interval-round (DATE-iv &optional ignore-weekend-p)
  "Returns the interval of DATEs [DATE1 DATE2]. If IGNORE-WEEKEND-P is
non-nil neither DATE1 nor DATE2 is allowed to be weekend days. Then
the left limit of DATE-IV is rounded backwards to nearest Friday and
the right limit is rounded forwards to nearest Monday. Note that DATE1
< DATE2 is required."
  (let ((a (first DATE-iv))
	(b (second DATE-iv)))
    (if (< b a) 
      (error "First date argument is greater than the second.")
      (list (if (DATE-weekend-p a) (DATE-round (first DATE-iv) 5 t) a)
	    (if (DATE-weekend-p b) (DATE-round (second DATE-iv) 1 nil) b)))))

(defun D-IV-R (time1 time2 &optional iw-p)
  (DATE-interval-iso (DATE-interval-round (DATE-interval (DATE time1) (DATE time2)) iw-p)))
;;(loop for day from 0 to 7 collect (D-IV-R (now :day day) (now :day (+ day 7)) t))
;;(D-IV-R (now) (now :day 7) t)

(defun DATE-iso (DATE)
  "Prints DATE in iso date format."
  (iso-date (DATE-decode DATE)))
;;(DATE-iso (DATE (now)))

(defun DATE-interval-iso (DATE-interval)
  (format "[%s %s]"
	  (DATE-iso (first DATE-interval))
	  (DATE-iso (second DATE-interval))))
;;(DATE-interval-iso (DATE-interval (now) (now :month 1)))

(defun* DATE-generate-sequence (from-DATE to-DATE &optional ignore-weekend-p)
  "Generates a list of DATEs in range [FROM-DATE TO-DATE]. If
IGNORE-WEEKEND-P is non-nil, then the resulting sequence will skip all
weekend dates. "
  (loop for D from from-DATE to to-DATE 
	if (not (and ignore-weekend-p
		     (DATE-weekend-p D)))
	collect D))
;;(DATE-generate-sequence (DATE (now)) (DATE (now :day 10)))
