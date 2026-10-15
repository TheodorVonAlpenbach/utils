;;; mb-utils-iso-time.el --- ISO date and time helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun numbers-regexp (a b)
  "Return a regexp matching zero-padded integers from A through B."
  (regexp-opt (mapcar (lambda (number) (format "%02d" number))
                      (number-sequence a b))))

(defconst +iso-year-regexp+ "[0-9][0-9][0-9][0-9]")
(defconst +iso-month-regexp+ "\\(?:0[1-9]\\|1[0-2]\\)")
(defconst +iso-day-regexp+ "\\(?:0[1-9]\\|[12][0-9]\\|3[01]\\)")

(defun iso-date-regexp ()
  "Return a regexp matching an ISO date with three numbered groups."
  (format "\\b\\(?1:%s\\)-\\(?2:%s\\)-\\(?3:%s\\)"
          +iso-year-regexp+ +iso-month-regexp+ +iso-day-regexp+))

(defun 0-23-regexp ()
  "Return a regexp matching a zero-padded hour from 00 through 23."
  "\\(?:[01][0-9]\\|2[0-3]\\)")

(defun 0-59-regexp ()
  "Return a regexp matching a zero-padded number from 00 through 59."
  "[0-5][0-9]")

(defun iso-time-hm-regexp (h-index m-index)
  "Return an ISO hour-minute regexp using groups H-INDEX and M-INDEX."
  (format "\\(?:\\(?%d:%s\\):\\(?%d:%s\\)\\|\\(?%d:24\\):\\(?%d:00\\)\\)"
          h-index (0-23-regexp) m-index (0-59-regexp)
          h-index m-index))

(cl-defun iso-time-zone-regexp (&optional (sign-index 1)
                                          (h-index (1+ sign-index))
                                          (m-index (1+ h-index)))
  "Return a regexp matching an ISO UTC offset.
SIGN-INDEX, H-INDEX, and M-INDEX select the capture groups for
the sign, hour, and minute components."
  (format
   "\\(?:\\(?%d:Z\\)\\|\\(?%d:[+-]\\)\\(?%d:%s\\)\\(?::?\\(?%d:%s\\)\\)?\\)$"
   sign-index sign-index h-index (0-23-regexp) m-index (0-59-regexp)))

(defun iso-time-regexp ()
  "Return a regexp matching an ISO 8601 time.
For the complete form `hh:mm:ss.ms+HH:MM', groups 4 through 10
contain hour, minute, second, fraction, sign, offset hour, and
offset minute."
  (format "%s\\(?::\\(?6:%s\\)\\(?:\\.\\(?7:[0-9]+\\)\\)?\\)?\\(?:%s\\)?\\b"
          (iso-time-hm-regexp 4 5)
          (0-59-regexp)
          (iso-time-zone-regexp 8)))

(defconst *iso-date* (iso-date-regexp))
(defconst *iso-time* (iso-time-regexp))

(defun iso-dttm-regexp (date-time-split-regexp)
  "Return a date-time regexp using DATE-TIME-SPLIT-REGEXP."
  (format "%s\\(?:%s%s\\)?\\|\\(?:%s\\)"
          *iso-date* date-time-split-regexp *iso-time* *iso-time*))

(defconst *iso-dttm* (iso-dttm-regexp "T"))
(defconst *iso-dttm-lazy* (iso-dttm-regexp "[T ]"))

(defconst *time-max-int* 134217727 "2^27 - 1")

(defconst *weekdays*
  '((:s0 (0 1 2 3 4 5 6) "Zero based weekday sequence starting on a Sunday")
    (:s1 (1 2 3 4 5 6 7) "One based weekday sequence starting on a Sunday")
    (:m0 (6 0 1 2 3 4 5) "Zero based weekday sequence starting on a Monday")
    (:m1 (7 1 2 3 4 5 6) "One based weekday sequence starting on a Monday")
    (:no ("søndag" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag")
         "Norwegian")
    (:en ("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
         "English")
    (:de ("Sonntag" "Montag" "Dienstag" "Mitwoch" "Donnerstag" "Freitag" "Samstag")
         "German")
    (:it ("domenica" "lunedi" "martedi" "mercoledi" "giovedi" "sabbato")
         "Italian")
    (:se ("söndag" "måndag" "tisdag" "onsdag" "torsdag" "fredag" "lördag")
         "Swedish")))

(cl-defun mb-weekday-languages (&optional (format :description)
                                          (skip-number-sequences-p t))
  "Return the supported weekday languages.
FORMAT may be `:description' or any other value for language
keys.  Unless SKIP-NUMBER-SEQUENCES-P is nil, omit numeric
weekday conventions."
  (mapcar (if (eq format :description) #'cl-third #'cl-first)
          (if skip-number-sequences-p
              (cl-remove-if (lambda (entry)
                              (integerp (cl-first (cl-second entry))))
                            *weekdays*)
            *weekdays*)))

(defconst *months*
  '((:j0 (0 1 2 3 4 5 6 7 8 9 10 11))
    (:j1 (1 2 3 4 5 6 7 8 9 10 11 12))
    (:no ("januar" "februar" "mars" "april" "mai" "juni" "juli"
          "august" "september" "oktober" "november" "desember"))
    (:en ("January" "February" "March" "April" "May" "June" "July"
          "August" "September" "October" "November" "December"))))

(cl-defun month-name (n &optional (lang :no) short-form
                        (short-form-size 3) (months *months*))
  "Return month N in language LANG from MONTHS.
When SHORT-FORM is non-nil, return at most SHORT-FORM-SIZE
characters."
  (let ((name (nth n (cl-second (assoc lang months)))))
    (if short-form
        (substring name 0 (min short-form-size (length name)))
      name)))

(defun mb-iso-time--encode (time-designator)
  "Convert TIME-DESIGNATOR to an encoded Emacs time value."
  (cond
   ((and (listp time-designator) (= (length time-designator) 9))
    (apply #'encode-time time-designator))
   ((consp time-designator)
    (copy-tree time-designator))
   ((stringp time-designator)
    (date-to-time
     (replace-regexp-in-string "T" " " time-designator nil t)))
   ((symbolp time-designator)
    (mb-iso-time--encode (symbol-name time-designator)))
   ((numberp time-designator)
    (seconds-to-time time-designator))
   (t
    (error "Invalid time designator: %S" time-designator))))

(defun mb-iso-time--decode (time-designator)
  "Convert TIME-DESIGNATOR to a decoded Emacs time value."
  (decode-time (mb-iso-time--encode time-designator)))

(cl-defun iso-date (&optional (time-designator (current-time)) simple-p)
  "Return TIME-DESIGNATOR as an ISO date string.
When SIMPLE-P is non-nil, omit dashes."
  (format-time-string (if simple-p "%Y%m%d" "%Y-%m-%d")
                      (mb-iso-time--encode time-designator)))

(cl-defun weekday (&optional (lang :no) (time-designator (current-time)))
  "Return the weekday of TIME-DESIGNATOR in language LANG."
  (nth (nth 6 (mb-iso-time--decode time-designator))
       (cl-second (assoc lang *weekdays*))))

(cl-defun full-date (&optional (lang :no) (time-designator (current-time)))
  "Return TIME-DESIGNATOR with weekday in language LANG."
  (format "%s %s"
          (weekday lang time-designator)
          (iso-date time-designator)))

(cl-defun short-date (&optional (time-designator (current-time)))
  "Return TIME-DESIGNATOR in day.month form."
  (interactive)
  (let ((decoded (mb-iso-time--decode time-designator)))
    (format "%d.%d" (nth 3 decoded) (nth 4 decoded))))

(cl-defun iso-time (&key (time (current-time)) with-seconds simple-p)
  "Return TIME as an ISO time string.
When WITH-SECONDS is non-nil, include seconds.  When SIMPLE-P is
non-nil, omit colons."
  (format-time-string
   (cond
    ((and with-seconds simple-p) "%H%M%S")
    (with-seconds "%H:%M:%S")
    (simple-p "%H%M")
    (t "%H:%M"))
   (mb-iso-time--encode time)))

(cl-defun iso-date-and-time (&key (time (current-time))
                                  with-seconds simple-p)
  "Return TIME as an ISO date-time string.
WITH-SECONDS and SIMPLE-P control seconds and punctuation."
  (concat (iso-date time simple-p)
          "T"
          (iso-time :time time
                    :with-seconds with-seconds
                    :simple-p simple-p)))

(provide 'mb-utils-iso-time)

;;; mb-utils-iso-time.el ends here
