;;; mb-things.el --- Editable things at point -*- lexical-binding: t; coding: utf-8; -*-

(require 'cl-lib)
(require 'thingatpt)

(defmacro defthing (name regexp)
  (let* ((symbol (if (and (consp name) (eq (car name) 'quote))
                     (cadr name)
                   name))
	 (bounds (gensym))
	 (n (gensym))
	 (as-list-p (gensym))
	 (point (gensym))
	 (sname (symbol-name symbol))
	 (name-regxp (intern (concat "*" sname "-regexp*"))) ;now obsolete?
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
		  (goto-char (cdr ,bounds))
		  (error "No %s here" ,sname))))))
       (put ',symbol 'beginning-op
	    (function (lambda ()
	      (let ((,bounds (,thing-at-point-bounds-of-name)))
		(if ,bounds
		  (goto-char (car ,bounds))
		  (error "No %s here" ,sname))))))

       (cl-defun ,forward-name (&optional (,n 1))
	 ,(format "Move forward across %s things.
With negative N, move backward." sname)
	 (interactive "p")
	 (unless (= ,n 0)
	   (let ((,point (point))
		 (,bounds (,thing-at-point-bounds-of-name)))
	     (if (> ,n 0)
	       ;; forward
	       (if (and ,bounds 
			(< ,point (cdr ,bounds)))
		 (progn (goto-char (cdr ,bounds))
			(re-search-forward ,name-regxp nil (- ,n 1)))
		 (re-search-forward ,name-regxp nil t ,n))
	       ;; backward
	       (if (and ,bounds 
			(< (car ,bounds) ,point))
		 (progn (goto-char (car ,bounds))
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
  "Note that if a cyclic element is contained in another the former
must be a member of a cycle preceding the cycle of the latter.")

;; Define cyclic things for each list element in *CYCLIC-THINGS*
;; Can't do this in a loop because the macro must take a symbol directly
(defthing 'weekday (regexp-opt (cadr (assoc 'weekday *cyclic-things*))))
(defthing 'ukedag (regexp-opt (cadr (assoc 'ukedag *cyclic-things*))))
(defthing 'month-short (regexp-opt (cadr (assoc 'month-short *cyclic-things*))))
(defthing 'month (regexp-opt (cadr (assoc 'month *cyclic-things*))))
(defthing 'måned (regexp-opt (cadr (assoc 'måned *cyclic-things*))))

;; Define every leaf node in *CYCLIC-THINGS* as a 'CYCLIC thing
(defthing 'cyclic (regexp-opt (mapcan #'cadr (copy-tree *cyclic-things*))))

(defthing 'date mb-things--iso-date-regexp)
(defthing 'time mb-things--iso-time-regexp)

(cl-defun cycle-type (string)
  "Return the cycle type when STRING is a `cyclic' thing."
  (car (cl-find string *cyclic-things*
                :test (lambda (candidate cycle)
                        (cl-find candidate cycle
                                 :test #'string-equal-ignore-case))
                :key #'cadr)))

(cl-defun replace-thing-at-point (new thing)
  "Replace THING at point with NEW."
  (let* ((bounds (or (bounds-of-thing-at-point thing)
		     (bounds-of-thing-at-point 'sexp)))
	 (offset (- (point) (car bounds)))
        (text (mb-things--string new)))
    (delete-region (car bounds) (cdr bounds))
    (insert text)
    (goto-char (+ (car bounds) (min offset (length text))))))
;;(definteractive number-at-point)

(cl-defun inc-thing-at-point (n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (let ((date (date-at-point t))
       (time (time-at-point t))
       (number (number-at-point))
       (cyclic (cyclic-at-point t)))
    (cond
    (date
    (save-excursion
      (replace-thing-at-point (inc-date date n level) 'date)
      (goto-char (car (bounds-of-thing-at-point 'sexp)))
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
      (goto-char (cdr (bounds-of-thing-at-point 'sexp)))
      (forward-sexp 1)
      (when (date-at-point)
	 (replace-thing-at-point (inc-date (thing-at-point 'date) n level) 'date))))  

    ((list-at-point t)) ; TODO

    ((word-at-point t))

    (t (error "No thing recognized")))))
(cl-defun inc-date (date n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (unless (string-match
           (concat "\\`" mb-things--iso-date-regexp "\\'")
           date)
    (error "Invalid ISO date: %S" date))
  (let ((year (string-to-number (substring date 0 4)))
        (month (string-to-number (substring date 5 7)))
        (day (string-to-number (substring date 8 10))))
    (pcase level
      (1 (cl-incf day n))
      (2 (cl-incf month n))
      (3 (cl-incf year n))
      (_ (error "Level %S is not implemented!" level)))
    (format-time-string "%Y-%m-%d"
                        (encode-time 0 0 12 day month year))))
;;(inc-date "2001-10-20" 1 1)

(cl-defun parse-clock (clock-string)
  "Parse CLOCK-STRING and return the time object for date 1970-01-01."
  (let ((parts (parse-time-string clock-string)))
    (encode-time (or (nth 0 parts) 0)
                 (or (nth 1 parts) 0)
                 (or (nth 2 parts) 0)
                 1 1 1970)))
;;(parse-clock "09:45")

(defun mb-things--clock-seconds (time)
  "Return seconds since midnight represented by TIME."
  (let ((parts (decode-time (parse-clock time))))
    (+ (nth 0 parts)
       (* 60 (nth 1 parts))
       (* 3600 (nth 2 parts)))))

(cl-defun inc-clock (time n level)
  "Increment clock string TIME N times according to LEVEL.
Numeric levels 1, 2, and 3 increment minutes, hours, and seconds.
A level of the form (UNIT QUANTITY) rounds in the direction of
QUANTITY and advances to the Nth boundary."
  (let* ((seconds (mb-things--clock-seconds time))
         (with-seconds (or (string-match-p ":[0-9][0-9]:" time)
                           (and (listp level) (eq (car level) :second))))
         result)
    (if (listp level)
        (progn
          (cl-assert (not (< n 0)) t "N cannot be negative")
          (cl-destructuring-bind (unit quantity) level
            (let* ((unit-details
                  (pcase unit
                      (:hour (list 3600 0 (/ seconds 3600)))
                      (:minute (list 60
                                     (* (/ seconds 3600) 3600)
                                     (% (/ seconds 60) 60)))
                      (:second (list 1
                                     (* (/ seconds 60) 60)
                                     (% seconds 60)))
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
    (if (or with-seconds (equal level 3))
        (format "%02d:%02d:%02d"
                (/ result 3600) (% (/ result 60) 60) (% result 60))
      (format "%02d:%02d" (/ result 3600) (% (/ result 60) 60)))))
;;(inc-clock "08:15:01" 1 3)
;;(inc-clock "08:15" 1 3)
 
(cl-defun inc-number (x n level)
  "Alter X N times according to LEVEL.
LEVEL 1, return N + X
LEVEL 2, add N to first digit in X
LEVEL 3, return x ^ (2 ^ N)
"
  (let ((res 
	 (cl-case level
	   (1 (+ n x))
	   (2 (+ x (* n (expt 10 (floor (log x 10))))))
	   (3 (expt x (expt 2.0 n))))))
    (if (integerp x) (round res) res)))
;;(inc-number 10 -1 3)

(cl-defun inc-cyclic (string n level &optional thing)
  (ignore level)
  (let* ((cycle-list (or thing
                         (cadr (assoc (cycle-type string)
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
