(require 'mb-utils-div)

(defmacro defthing (name regexp)
  (let* ((symbol (eval name))		;perhaps unnecessary
	 (bounds (gensym))
	 (n (gensym))
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

      (defun ,thing-at-point-bounds-of-name ()
	(when (thing-at-point-looking-at ,name-regxp)
	  (cons (match-beginning 0) (match-end 0))))

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

      (defun ,name-at-point () 
	,(format "%s is generated with macro `defthing' in module 'mb-things." name-at-point)
	(thing-at-point ',symbol))

      (defun ,type-name (string)
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
(loop for x in *cyclic-things*
      for name = (first x)
      for regexp = (regexp-opt (second x))
      do (defthing name regexp))

;; Define every leaf node in *CYCLIC-THINGS* as a 'CYCLIC thing
(defthing 'cyclic (regexp-opt (mapcan #'second (copy-tree *cyclic-things*))))

(defthing 'date *iso-date*)
(defthing 'time *iso-time*)
;;(defthing 'weekday-and-date *weekday-and-date*)

(defun cycle-type (string)
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
    (insert new)
    (goto-char (min (cdr bounds) point))))
;;(definteractive number-at-point)

(cl-defun inc-thing-at-point (n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (acond
    ;; ((weekday-and-date-at-point) (inc-weekday-and-date-at-point n level))
    ((date-at-point) (save-excursion
		       (replace-thing-at-point (inc-date it n level) 'date)
		       (beginning-of-sexp)
		       (backward-sexp 1)
		       (when (or (weekday-at-point) (ukedag-at-point))
			 (replace-thing-at-point (inc-cyclic (thing-at-point 'cyclic) n level) 'cyclic))))
    ((time-at-point) (replace-thing-at-point (inc-time it n level) 'time))
    ((number-at-point) (replace-thing-at-point (inc-number it n level) 'number))
    ((cyclic-at-point) (save-excursion
			 (replace-thing-at-point (inc-cyclic it n level) 'cyclic)
			 (end-of-sexp)
			 (forward-sexp 1)
			 (when (date-at-point)
			   (replace-thing-at-point (inc-date (thing-at-point 'date) n level) 'date))))  
    ((list-at-point)) ;;todo
    ((word-at-point))
    (t (error "No thing recoginzed"))))

(defun inc-weekday-and-date-at-point (n level)
  "Doesn't work thing-at-point doesn't seem to handle spaces in regexps"
  (let ((original-point (point)) 
	(bounds (thing-at-point-bounds-of-weekday-and-date)))
    (goto-char (first bounds))
    (replace-thing-at-point (inc-cyclic (cyclic-at-point) n level) 'cyclic)
    (goto-char (rest bounds))
    (replace-thing-at-point (inc-date (date-at-point) n level) 'date)
    (goto-char original-point)))
;;(inc-weekday-and-date (weekday-and-date-at-point) n level)

(defun inc-date (date n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (iso-date (case level
	      (1 (add-time date :day n))
	      (2 (add-time date :month n))
	      (3 (add-time date :year n))
	      (otherwise (error "Level %d is not implemented!" level)))))
;;(inc-date "2001-10-20" 1 1)

(defun inc-time (time n level)
  "LEVEL 1, 2, 3 correspond to day, month, year respectively."
  (iso-time :time (case level
		    (1 (add-time time :minute n))
		    (2 (add-time time :hour n))
		    (3 (add-time time :second n))
		    (otherwise (error "Level %d is not implemented!" level)))
	    :with-seconds (or (= level 3) 
			      (> (length time) 6))))
;;(inc-time "13:13" 1 3)

(defun inc-number (x n level)
  (let ((res 
	 (case level
	   (1 (+ n x))			;inc/dec X N times
	   (2 (dotimes (i n x)		;inc/dec first digit in X N times
		(incf x (* (signum n) (expt 10 (floor (log x 10)))))))
	   (3 (expt x (expt 2.0 n)))))) ;sqr/sqrt X N times
    (number-to-string (if (integerp x) (round res) res))))
;;(inc-number 9 -1 1)

(defun inc-cyclic (string n level &optional thing)
  (let* ((cycle-list (or thing (second (assoc (cycle-type string)
					      *cyclic-things*)))))
    (copy-case
     (nth (mod (+ n (position string cycle-list :test #'string-equal*))
	       (length cycle-list))
	  cycle-list)
     string)))
;;(inc-cyclic "2004-03-29" 2 1)

;;;;;; templates
; (defun thing-at-point-bounds-of-lynx-reference ()
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
