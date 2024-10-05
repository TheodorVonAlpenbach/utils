(require 'mb-utils-div)

(defmacro defthing (name regexp)
  (let* ((symbol (ssymbol name))	;perhaps unnecessary
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
	 (type-name (intern (concat sname "p"))))
    `(progn
       (defconst ,name-regxp ,regexp (format "Regular expression for %d" ,sname))

       (cl-defun ,thing-at-point-bounds-of-name (&optional ,as-list-p)
	 ,(format
	      "Return the region bounds of the %s at point.
By default the bounds are returned as the cons pair (START END).
If optional AS-LIST-P is not nil, the bounds are returned as a list pair.

This function is generated with macro `defthing' in module
'mb-things.
\n(fn &optional AS-LIST-P)"
	    name)
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
	 ,(format "%s is generated with macro `defthing' in module 'mb-things. TODO: Handle n = 0."
	    forward-name)
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
	 ,(format "%s is generated with macro `defthing' in module 'mb-things. TODO: Handle n = 0."
	    backward-name)
	 (interactive "p")
	 (,forward-name (- ,n)))

       (cl-defun ,name-at-point (&optional no-properties) 
	 ,(format "%s is generated with macro `defthing' in module 'mb-things." name-at-point)
	 (thing-at-point ',symbol no-properties))

       (cl-defun ,type-name (string)
	 ,(format "%s is generated with macro `defthing' in module 'mb-things." type-name)
	 (integerp (string-match ,regexp string)))
      
       (definteractive ,name-at-point))))

;;;; mb definitions
(require 'mb-utils-time)

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

(defconst *weekday-and-date*
  (concat* (append (second (assoc 'weekday *cyclic-things*))
		   (second (assoc 'ukedag *cyclic-things*)))
	   :in "\\|"
	   :suf (concat "\\s-w*" *iso-date*)))

;; Define cyclic things for each list element in *CYCLIC-THINGS*
;; Can't do this in a loop because the macro must take a symbol directly
(defthing 'weekday (regexp-opt (second (assoc 'weekday *cyclic-things*))))
(defthing 'ukedag (regexp-opt (second (assoc 'ukedag *cyclic-things*))))
(defthing 'month-short (regexp-opt (second (assoc 'month-short *cyclic-things*))))
(defthing 'month (regexp-opt (second (assoc 'month *cyclic-things*))))
(defthing 'måned (regexp-opt (second (assoc 'måned *cyclic-things*))))

;; Define every leaf node in *CYCLIC-THINGS* as a 'CYCLIC thing
(defthing 'cyclic (regexp-opt (mapcan #'second (copy-tree *cyclic-things*))))

(defthing 'date *iso-date*)
(defthing 'time *iso-time*)
;;(defthing 'weekday-and-date *weekday-and-date*)

(cl-defun cycle-type (string)
  "If STRING is 'CYCLIC it returns the cycle type."
  (first (find string *cyclic-things* 
	       :test #'(lambda (o e) (find o e :test #'string-equal*))
	       :key #'second)))

(cl-defun replace-thing-at-point (new thing)
  "Replace THING at point with NEW \(a string\)"
  (let* ((bounds (or (bounds-of-thing-at-point thing)
		     (bounds-of-thing-at-point 'sexp)))
	 (point (point))
	 (offset (- (car bounds) (point))))
    (delete-region (car bounds) (cdr bounds))
    (insert (sstring new))
    (goto-char (min (cdr bounds) point))))
;;(definteractive number-at-point)

(cl-defun inc-thing-at-point (n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (acond
    ;; ((weekday-and-date-at-point) (inc-weekday-and-date-at-point n level))
    ((date-at-point t)
     (save-excursion
       (replace-thing-at-point (inc-date it n level) 'date)
       (beginning-of-sexp)
       (backward-sexp 1)
       (when (or (weekday-at-point) (ukedag-at-point))
	 (replace-thing-at-point (inc-cyclic (thing-at-point 'cyclic) n level) 'cyclic))))

    ((time-at-point t)
     (replace-thing-at-point (inc-clock it n level) 'time))

    ((number-at-point)
     (replace-thing-at-point (inc-number it n level) 'number))

    ((cyclic-at-point t)
     (save-excursion
       (replace-thing-at-point (inc-cyclic it n level) 'cyclic)
       (end-of-sexp)
       (forward-sexp 1)
       (when (date-at-point)
	 (replace-thing-at-point (inc-date (thing-at-point 'date) n level) 'date))))  

    ((list-at-point t)) ;;todo

    ((word-at-point t))

    (t (error "No thing recoginzed"))))
;; 2000  (inc-thing-at-point -1 2)

(cl-defun inc-weekday-and-date-at-point (n level)
  "Doesn't work thing-at-point doesn't seem to handle spaces in regexps"
  (let ((original-point (point)) 
	(bounds (thing-at-point-bounds-of-weekday-and-date)))
    (goto-char (first bounds))
    (replace-thing-at-point (inc-cyclic (cyclic-at-point) n level) 'cyclic)
    (goto-char (rest bounds))
    (replace-thing-at-point (inc-date (date-at-point) n level) 'date)
    (goto-char original-point)))
;;(inc-weekday-and-date (weekday-and-date-at-point) n level)

(cl-defun inc-date (date n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (iso-date (cl-case level
	      (1 (add-etime-date (parse-time date) :day n))
	      (2 (add-etime-date (parse-time date) :month n))
	      (3 (add-etime-date (parse-time date) :year n))
	      (otherwise (error "Level %d is not implemented!" level)))))
;;(inc-date "2001-10-20" 1 1)

(cl-defun parse-clock (clock-string)
  "Parse clock-string and return the time object for date 1970-01-01."
  (apply #'encode-time
    (append (subseq (parse-time-string clock-string) 0 3)
	    (list 1 1 1970))))
;;(parse-clock "09:45")

(cl-defun inc-clock (time n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively.
TIME must be a string."
  (let ((etime (parse-clock time)))
    (iso-time
     :time (if (listp level)
	     (cl-destructuring-bind (unit quantity) level
	       (cl-case (first level)
		 (:hour (etime-round etime :hour quantity n))
		 (:minute (etime-round etime :minute quantity n))
		 (:second (etime-round etime :second quantity n))
		 (otherwise
		  (error "Level unit %S is not implemented!" unit))))
	     (cl-case level
	       (1 (add-time etime :minute n))
	       (2 (add-time etime :hour n))
	       (3 (add-time etime :second n))
	       (otherwise
		(error "Level %S is not implemented!" level))))
     :with-seconds (if (listp level)
		     (eql (first level) :second)
		     (or (= level 3) (> (length time) 6))))))
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
  (let* ((cycle-list (or thing (second (assoc (cycle-type string)
					      *cyclic-things*)))))
    (copy-case
     (nth (mod (+ n (position string cycle-list :test #'string-equal*))
	       (length cycle-list))
	  cycle-list)
     string)))
;;(inc-cyclic "2004-03-29" 2 1)

;;;;;; templates
; (cl-defun thing-at-point-bounds-of-lynx-reference ()
;   (when (thing-at-point-looking-at *lynx-ref-regexp*)
;     (cons (match-beginning 0) (match-end 0))))
; (put 'lynx-reference 'end-op
;      (function (lambda ()
;        (let ((bounds (thing-at-point-bounds-of-lynx-reference)))
; 	 (if bounds
; 	   (goto-char (cdr bounds))
; 	   (error "No URL here"))))))
; (put 'lynx-reference 'beginning-op
;      (function (lambda ()
; 		 (let ((bounds (thing-at-point-bounds-of-lynx-reference)))
; 		   (if bounds
; 		       (goto-char (car bounds))
; 		     (error "No URL here"))))))
; (cl-defun forward-lynx-reference (&optional (n 1))
;   "TODO: Handle n = 0."
;   (interactive "p")
;   (unless (= n 0)
;     (let ((point (point))
; 	  (bounds (thing-at-point-bounds-of-lynx-reference)))
;       (unless 
; 	  (if (> n 0)
; 	    (if (and bounds (< point (cdr bounds)))
; 	      (and (goto-char (cdr bounds))
; 		   (re-search-forward *lynx-ref-regexp* nil t (- n 1)))
; 	      (re-search-forward *lynx-ref-regexp* nil t n))
; 	    ;; backward
; 	    (if (and bounds (< (car bounds) point))
; 	      (and (goto-char (car bounds))
; 		   (re-search-backward *lynx-ref-regexp* nil t (- 1 n))))
; 	    (re-search-backward *lynx-ref-regexp* nil t (- n)))
; 	(goto-char point))
;       (point))))

(provide 'mb-things)
