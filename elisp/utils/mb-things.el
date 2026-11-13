;;; mb-things.el --- Editable things at point -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;; Define `thing-at-point' recognizers and increment dates, clocks, numbers, and
;; cyclic names such as weekdays and months.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(defmacro defthing (name regexp)
  "Define NAME as a thing recognized by REGEXP.
Generate movement, bounds, predicate, and `thing-at-point' functions."
  (let* ((symbol (if (and (consp name) (eq (cl-first name) 'quote))
                     (cl-second name)
                   name))
	 (bounds (gensym))
	 (n (gensym))
	 (as-list-p (gensym))
	 (point (gensym))
	 (sname (symbol-name symbol))
	 (name-regxp (intern (concat "*" sname "-regexp*")))
	 (thing-at-point-bounds-of-name (intern (concat "thing-at-point-bounds-of-" sname)))
	 (forward-name (intern (concat "forward-" sname)))
	 (backward-name (intern (concat "backward-" sname)))
	 (name-at-point (intern (concat sname "-at-point")))
	 (type-name (intern (concat sname "p")))
         (interactive-name (intern (concat sname "-at-point*"))))
    (unless (symbolp symbol)
      (error "Thing name must be a symbol: %S" name))
    `(progn
       (defconst ,name-regxp ,regexp ,(format "Regular expression for %s." sname))

       (cl-defun ,thing-at-point-bounds-of-name (&optional ,as-list-p)
	 ,(format
	      "Return the region bounds of the %s at point.
By default the bounds are returned as the cons pair (START END).
If optional AS-LIST-P is not nil, the bounds are returned as a list pair.

This function is generated with macro `defthing' in module
`mb-things'.
\n(fn &optional AS-LIST-P)"
	    sname)
       	 (when (thing-at-point-looking-at ,name-regxp)
	   (if ,as-list-p
	     (list (match-beginning 0) (match-end 0))
	     (cons (match-beginning 0) (match-end 0)))))

       (put ',symbol 'end-op
	    (function (lambda ()
	      (let ((,bounds (,thing-at-point-bounds-of-name)))
		(if ,bounds
		  (goto-char (cl-rest ,bounds))
		  (error "No %s here" ,sname))))))
       (put ',symbol 'beginning-op
	    (function (lambda ()
	      (let ((,bounds (,thing-at-point-bounds-of-name)))
		(if ,bounds
		  (goto-char (cl-first ,bounds))
		  (error "No %s here" ,sname))))))

       (cl-defun ,forward-name (&optional (,n 1))
	 ,(format "Move forward across %s things.
With negative N, move backward." sname)
	 (interactive "p")
	 (cl-check-type ,n integer)
	 (unless (= ,n 0)
	   (let ((,point (point))
		 (,bounds (,thing-at-point-bounds-of-name)))
	     (if (> ,n 0)
	       ;; forward
	       (if (and ,bounds
			(< ,point (cl-rest ,bounds)))
		 (progn (goto-char (cl-rest ,bounds))
			(re-search-forward ,name-regxp nil (- ,n 1)))
		 (re-search-forward ,name-regxp nil t ,n))
	       ;; backward
	       (if (and ,bounds
			(< (cl-first ,bounds) ,point))
		 (progn (goto-char (cl-first ,bounds))
			(re-search-backward ,name-regxp nil t (- (abs ,n) 1)))
		 (re-search-backward ,name-regxp nil t (- ,n)))))))

       (cl-defun ,backward-name (&optional (,n 1))
	 ,(format "Move backward across %s things.
With negative N, move forward." sname)
	 (interactive "p")
	 (,forward-name (- ,n)))

       (cl-defun ,name-at-point (&optional no-properties)
	 ,(format "Return the %s at point." sname)
	 (thing-at-point ',symbol no-properties))

       (cl-defun ,type-name (string)
	 ,(format "Return non-nil when STRING contains a %s." sname)
	 (integerp (string-match ,regexp string)))

       (cl-defun ,interactive-name ()
         ,(format "Interactively print the %s at point." sname)
         (interactive)
         (princ (,name-at-point))))))

;;;; mb definitions

(defconst mb-things--iso-date-regexp
  "\\b[0-9]\\{4\\}-\\(?:0[1-9]\\|1[0-2]\\)-\\(?:0[1-9]\\|[12][0-9]\\|3[01]\\)\\b")

(defconst mb-things--iso-time-regexp
  (concat
   "\\b\\(?:"
   "\\(?:[01][0-9]\\|2[0-3]\\):[0-5][0-9]"
   "\\(?::[0-5][0-9]\\(?:\\.[0-9]+\\)?\\)?"
   "\\|24:00\\(?::00\\(?:\\.0+\\)?\\)?"
   "\\)"
   "\\(?:Z\\|[+-]\\(?:[01][0-9]\\|2[0-3]\\)\\(?::?[0-5][0-9]\\)?\\)?\\b"))

(defconst mb-things--clock-parts-regexp
  (concat
   "\\`\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
   "\\(?::\\([0-9][0-9]\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?"
   "\\(Z\\|[+-][0-9][0-9]\\(?::?[0-9][0-9]\\)?\\)?\\'")
  "Regexp capturing the components of a validated ISO clock string.")

(defun mb-things--string (value)
  "Return VALUE as a string."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun mb-things--copy-case (string pattern)
  "Return STRING with capitalization resembling PATTERN."
  (cond
   ((string= pattern (upcase pattern)) (upcase string))
   ((and (> (length pattern) 0)
         (= (aref pattern 0) (upcase (aref pattern 0))))
    (capitalize string))
   (t (downcase string))))

(defconst *cyclic-things*
  '((weekday
     ("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"))
    (ukedag
     ("mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag" "søndag"))
    (month-short
     ("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))
    (month
     ("january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december"))
    (måned
     ("januar" "februar" "mars" "april" "mai" "juni" "juli" "august" "september" "oktober" "november" "desember")))
  "Cycles used by `inc-cyclic'.
When one element contains another, put the shorter element's cycle first.")

;; Define cyclic things for each list element in *CYCLIC-THINGS*
;; Can't do this in a loop because the macro must take a symbol directly
(defthing 'weekday (regexp-opt (cl-second (assoc 'weekday *cyclic-things*))))
(defthing 'ukedag (regexp-opt (cl-second (assoc 'ukedag *cyclic-things*))))
(defthing 'month-short (regexp-opt (cl-second (assoc 'month-short *cyclic-things*))))
(defthing 'month (regexp-opt (cl-second (assoc 'month *cyclic-things*))))
(defthing 'måned (regexp-opt (cl-second (assoc 'måned *cyclic-things*))))

;; Define every leaf node in *CYCLIC-THINGS* as a 'CYCLIC thing
(defthing 'cyclic (regexp-opt (mapcan #'cl-second (copy-tree *cyclic-things*))))

(defthing 'date mb-things--iso-date-regexp)
(defthing 'time mb-things--iso-time-regexp)

(cl-defun cycle-type (string)
  "Return the cycle type when STRING is a `cyclic' thing."
  (cl-first (cl-find string *cyclic-things*
                :test (lambda (candidate cycle)
                        (cl-find candidate cycle
                                 :test #'string-equal-ignore-case))
                :key #'cl-second)))

(cl-defun replace-thing-at-point (new thing)
  "Replace THING at point with NEW."
  (let* ((bounds (or (bounds-of-thing-at-point thing)
		     (bounds-of-thing-at-point 'sexp))))
    (unless bounds
      (error "No %s or expression at point" thing))
    (let* (
	 (offset (- (point) (cl-first bounds)))
	 (text (mb-things--string new)))
      (delete-region (cl-first bounds) (cl-rest bounds))
      (insert text)
      (goto-char (+ (cl-first bounds) (min offset (length text)))))))

(cl-defun inc-thing-at-point (n level)
  "Increment the recognized thing at point N times according to LEVEL.
For dates, numeric levels 1, 2, and 3 select day, month, and year."
  (let ((date (date-at-point t))
       (time (time-at-point t))
       (number (number-at-point))
       (cyclic (cyclic-at-point t)))
    (cond
    (date
    (save-excursion
      (replace-thing-at-point (inc-date date n level) 'date)
      (goto-char (cl-first (bounds-of-thing-at-point 'sexp)))
      (backward-sexp 1)
      (when (or (weekday-at-point) (ukedag-at-point))
	 (replace-thing-at-point (inc-cyclic (thing-at-point 'cyclic) n level) 'cyclic))))

    (time
    (replace-thing-at-point (inc-clock time n level) 'time))

    (number
    (replace-thing-at-point (inc-number number n level) 'number))

    (cyclic
    (save-excursion
      (replace-thing-at-point (inc-cyclic cyclic n level) 'cyclic)
      (goto-char (cl-rest (bounds-of-thing-at-point 'sexp)))
      (forward-sexp 1)
      (when (date-at-point)
	 (replace-thing-at-point
	  (inc-date (thing-at-point 'date) n level)
	  'date))))

    (t (error "No incrementable thing recognized")))))

(cl-defun inc-date (date n level)
  "Increment ISO DATE by N units selected by LEVEL.
Levels 1, 2, and 3 select day, month, and year respectively."
  (unless (string-match
           (concat "\\`" mb-things--iso-date-regexp "\\'")
           date)
    (error "Invalid ISO date: %S" date))
  (cl-check-type n integer)
  (let ((year (string-to-number (substring date 0 4)))
        (month (string-to-number (substring date 5 7)))
        (day (string-to-number (substring date 8 10))))
    (unless (equal
	     (format-time-string "%Y-%m-%d"
				 (encode-time 0 0 12 day month year))
	     date)
      (error "Invalid calendar date: %S" date))
    (pcase level
      (1 (cl-incf day n))
      (2 (cl-incf month n))
      (3 (cl-incf year n))
      (_ (error "Level %S is not implemented!" level)))
    (format-time-string "%Y-%m-%d"
                        (encode-time 0 0 12 day month year))))

(defun mb-things--clock-components (time)
  "Return parsed components of validated ISO clock string TIME."
  (unless (and (stringp time)
	       (string-match-p
		(concat "\\`" mb-things--iso-time-regexp "\\'")
		time)
	       (string-match mb-things--clock-parts-regexp time))
    (error "Invalid ISO time: %S" time))
  (let ((fraction (match-string 4 time)))
    (list (string-to-number (match-string 1 time))
	  (string-to-number (match-string 2 time))
	  (+ (string-to-number (or (match-string 3 time) "0"))
	     (if fraction
		 (string-to-number (concat "0." fraction))
	       0))
	  fraction
	  (match-string 5 time)
	  (not (null (match-string 3 time))))))

(cl-defun parse-clock (clock-string)
  "Parse CLOCK-STRING and return the time object for date 1970-01-01."
  (cl-destructuring-bind (hour minute second _fraction zone _seconds-p)
      (mb-things--clock-components clock-string)
    (encode-time second minute hour 1 1 1970
		 (cond
		  ((equal zone "Z") 0)
		  (zone (nth 8 (parse-time-string clock-string)))
		  (t nil)))))

(defun mb-things--clock-seconds (time)
  "Return seconds since midnight represented by TIME."
  (cl-destructuring-bind (hour minute second _fraction _zone _seconds-p)
      (mb-things--clock-components time)
    (+ second (* 60 minute) (* 3600 hour))))

(cl-defun inc-clock (time n level)
  "Increment clock string TIME N times according to LEVEL.
Numeric levels 1, 2, and 3 increment minutes, hours, and seconds.
A level of the form (UNIT QUANTITY) rounds in the direction of
QUANTITY and advances to the Nth boundary."
  (cl-check-type n integer)
  (cl-destructuring-bind (_hour _minute _second fraction zone seconds-p)
      (mb-things--clock-components time)
    (let* ((seconds (mb-things--clock-seconds time))
         (with-seconds (or seconds-p
			   (equal level 3)
			   (and (listp level)
				(eq (cl-first level) :second))))
         result)
    (if (listp level)
        (progn
          (unless (and (proper-list-p level) (= (length level) 2))
	    (error "Boundary level must be (UNIT QUANTITY): %S" level))
	  (unless (cl-plusp n)
	    (error "Boundary increment count must be positive: %S" n))
          (cl-destructuring-bind (unit quantity) level
	    (unless (and (integerp quantity) (not (zerop quantity)))
	      (error "Boundary quantity must be a nonzero integer: %S"
		     quantity))
            (let* ((unit-details
                  (pcase unit
                      (:hour (list 3600 0 (floor seconds 3600)))
                      (:minute (list 60
                                     (* (floor seconds 3600) 3600)
                                     (mod (floor seconds 60) 60)))
                      (:second (list 1
                                     (* (floor seconds 60) 60)
                                     (- seconds
					(* (floor seconds 60) 60))))
                      (_ (error "Level unit %S is not implemented!" unit))))
                   (unit-seconds (nth 0 unit-details))
                   (higher-units (nth 1 unit-details))
                   (current-unit (nth 2 unit-details))
                   (step (* quantity unit-seconds))
                   (magnitude (abs quantity))
                   (boundary
                    (if (> quantity 0)
                        (* (1+ (floor current-unit magnitude)) magnitude)
                      (* (1- (ceiling current-unit magnitude)) magnitude))))
              (setq result (+ higher-units
                              (* boundary unit-seconds)
                              (* (1- n) step))))))
      (setq result
            (+ seconds
               (* n
                  (pcase level
                    (1 60)
                    (2 3600)
                    (3 1)
                    (_ (error "Level %S is not implemented!" level)))))))
    (setq result (mod result (* 24 60 60)))
    (let* ((whole (floor result))
	   (suffix (or zone ""))
	   (fraction-suffix
	    (if (and fraction (not (listp level)))
		(concat "." fraction)
	      "")))
      (if with-seconds
	  (format "%02d:%02d:%02d%s%s"
		  (/ whole 3600)
		  (% (/ whole 60) 60)
		  (% whole 60)
		  fraction-suffix
		  suffix)
	(format "%02d:%02d%s"
		(/ whole 3600)
		(% (/ whole 60) 60)
		suffix))))))

(cl-defun inc-number (x n level)
  "Alter X N times according to LEVEL.
At level 1, return N + X.  At level 2, add N to the first
significant digit of X.  At level 3, return X raised to 2 raised
to N."
  (cl-check-type x number)
  (cl-check-type n integer)
  (let ((res
	 (pcase level
	   (1 (+ n x))
	   (2 (let* ((sign (if (< x 0) -1 1))
		     (magnitude (abs x))
		     (place (if (zerop magnitude)
				1
			      (expt 10 (floor (log magnitude 10))))))
		(+ x (* sign n place))))
	   (3 (expt x (expt 2.0 n)))
	   (_ (error "Level %S is not implemented!" level)))))
    (if (integerp x) (round res) res)))

(cl-defun inc-cyclic (string n level &optional thing)
  "Increment cyclic STRING by N positions.
LEVEL is accepted for compatibility and ignored.  THING may supply
the cycle explicitly."
  (ignore level)
  (cl-check-type n integer)
  (let* ((cycle-list (or thing
                         (cl-second (assoc (cycle-type string)
                                           *cyclic-things*))))
         (position (cl-position string cycle-list
                                :test #'string-equal-ignore-case)))
    (unless position
      (error "%S is not a known cyclic value" string))
    (mb-things--copy-case
     (nth (mod (+ n position) (length cycle-list)) cycle-list)
     string)))
;;(inc-cyclic "2004-03-29" 2 1)

(provide 'mb-things)

;;; mb-things.el ends here
