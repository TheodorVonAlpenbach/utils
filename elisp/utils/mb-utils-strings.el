(require 'mb-utils-div)
(require 'mb-utils-sets)
(require 'mb-sequences)
(require 'mb-utils-math)

(cl-defun string> (string1 string2)
  "Returns non-nil iif string1 > string2. See `string<'"
  (string< string2 string1))

(cl-defun string<= (string1 string2)
  "Returns non-nil iif string1 > string2. See `string<'"
  (not (string> string1 string2)))

(cl-defun string>= (string1 string2)
  "Returns non-nil iif string1 > string2. See `string<'"
  (not (string< string1 string2)))

(cl-defun string-equal* (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Same as Common Lisp's string-equal."
;;   (string-equal (upcase (substring string1 start1 end1))
;; 		   (upcase (substring string2 start2 end2)))
  (eq t (compare-strings string1 start1 end1 string2 start2 end2 t)))
;;(string-equal* "abc" "BC" :start1 1 :end1 2 :start2 0 :end2 2)

(cl-defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (not (string-equal* string1 string2 
		      :start1 start1 :end1 end1 
		      :start2 start2 :end2 end2)))
;;(string/= "qwe" "qwe")

;;; Ignore case versions
(cl-defun istring< (string1 string2)
  (string-collate-lessp string1 string2 nil t))
;;(list (string< "qwe" "Rty") (istring< "qwe" "rty"))

(cl-defun istring= (string1 string2)
  (string-collate-equalp string1 string2 nil t))
;;(list (string= "qwe" "QWE") (istring= "qwe" "QWE"))

(cl-defun lower-case-p (character)
  "Common Lisp function. Checks if CHARACTER is invariant under
case-table."
  (= (aref (current-case-table) character) character))
;;(lower-case-p ?Æ) => nil

(cl-defun upper-case-p (character)
  "Inverse of `lower-case-p'."
  (not (lower-case-p character)))

(cl-defun integer-string-p (string &optional only-non-negative-p)
  (and (stringp string)
       (string-match-exact
	(if only-non-negative-p "[0-9]+" "-?[0-9]+") string)))
;;(mapcar #'integer-string-p '("123" "-123" 123 "a"))

(cl-defun string-to-integer (string)
  "Similar to `string-to-number', but returns nil (not 0) if the
STRING is not an integer, otherwise returns the resulting integer."
  (and (integer-string-p string)
       (string-to-number string)))
;;(mapcar #'integer-to-string '("123" 123 "a"))

(defmacro char (string index)
  "Common Lisp CHAR"
  `(aref ,string ,index))

(cl-defun string-to-character-list (string) (cl-coerce string 'list))
;;(string-to-character-list "abc")

(cl-defun copy-case (string pattern)
  "Returns copy of STRING that has similar case as PATTERN: If PATTERN
starts with two upper case characters, the entire result is upcased. If
PATTERN starts with an upcase character followed by a downcase, the
result is capitalized. Else the entire result is downcased."
  (if (upper-case-p (char pattern 0))
    (if (upper-case-p (char pattern 1))
      (upcase string)
      (capitalize string))
    (downcase string)))

(cl-defun pure-string (string)
  "Removes formatting from STRING and returns result."
  (cl-typecase string
    (sequence (elt string 0))
    (string string)
    (otherwise (error "unknown string format"))))

(cl-defun blanks (n &optional (char 32))
  "Return a sting made up of N spaces.
This is a shortcut for \(make-string N 32\))"
  (make-string n 32))

(cl-defun empty-string-p (string)
  (and (stringp string)
       (zerop (length string))))
;;(empty-string-p "")

(cl-defun not-empty (string)
  "Returns 'nil if STRING is empty. Otherwise returns STRING. Useful
in connection with e.g. `or'"
  (and (not (empty-string-p string))
       string))
;(not-empty "")

(cl-defun blank-p (string)
  "Return not nil if string has no non-whitespace characters."
  (empty-string-p (string-trim string)))

(cl-defun string-match-end (regexp string &optional start)
  "Same as `string-match', except that it returns the position after
match."
  (let ((pos (string-match regexp string)))
    (if pos (match-end 0) nil)))

(cl-defun split-string-at-pos (string &rest positions)
  (warn "split-string-at-pos is deprecated.
Use `split-at-position' instead. Called from %s"
	(second (backtrace-frame 6)))
  (apply #'split-at-position string positions))
;;(length (split-string-at-pos "qweqweqwe" 1))

(cl-defun split-string-by-length (string n &key (from-end nil))
  "Splits STRING to substrings of length N.
Only the length of the last substring (or the first substring, if
from-end is t) is less than (or equal to) N."
  (let ((split-positions (a-b (if from-end (mod (length string) n) n)
			      (length string) n)))
    (remove "" (apply #'split-at-position string split-positions))))
;;(split-string-by-length "0123456789012345" 3 :from-end t)

(cl-defun regexp-consecutive-matches (regexp string)
  "Returns the position boundaries of the consecutive substrings of
STRING that matches REGEXP"
  (cl-loop with position = 0
	while (string-match regexp string position)
	do (setq position (match-end 0))
	collect (list (match-beginning 0) (match-end 0))))
;;(regexp-consecutive-matches "a" "aasdfaa")

(cl-defun split-string-modify-positions (positions with-separator)
  (cl-case with-separator
    ((:none nil) (cut (cons 0 (flatten positions)) 2 t))
    (:left (cut (cons 0 (repeat-elements (mapcar #'first positions))) 2 t))
    (:right (cut (cons 0 (repeat-elements (mapcar #'second positions))) 2 t))
    (:only positions)))

(cl-defun split-string* (string &optional separators (with-separator :none))
  "If WITH-SEPARATOR is nil then the method is equal to
`split-string'. If WITH-SEPARATOR is :left then the matched
string is prefixed to the split result at that point. A similar
effect is achived with value :right. If WITH-SEPARATOR is :only,
only the matching separators are returned"
  (mapcar #'(lambda (x) 
	      (substring string (first x) (second x))) 
	  (split-string-modify-positions
	   (regexp-consecutive-matches (or separators "[ \f\t\n\r\v]+") string)
	   with-separator)))
;;(split-string* " 09:16  Text" "[0-2][0-9]:[0-5][0-9]" :none)

(cl-defun split-string-2 (string regexp &optional (endp nil) (with-separator t)) 
  "Splits STRING into strings at beginning of first match with REGEXP. if
ENDP is non-nil, the split is at end of regexp match."
  (if with-separator
    (let* ((pos (if endp
		  (string-match-end regexp string)
		  (string-match regexp string))))
      (values (substring string 0 pos)
	      (if pos (substring string pos) "")
	      pos))
    (let ((res-first (split-string-2 string regexp nil))
	  (res-second (split-string-2 string regexp t)))
      (values (first res-first)
	      (second res-second)
	      (third res-first)
	      (third res-second)))))
;;(split-string-2 "abcdef" "g" nil t)

(cl-defun split-string-3 (string regexp-interval)
  "Splits STRING into (BEG MID END), where MIN is first match of
REGEXP-INTERVAL in STRING. If the regexps in REGEXP-INTERVAL overlap,
the result is undefined."
  (cl-multiple-value-bind
  (beg rest match1)
  (split-string-2 string
		  (interval-l regexp-interval)
		  (open-left-p regexp-interval))
  (cl-multiple-value-bind 
      (mid end match2)
      (split-string-2 (if match1 rest beg)
		      (interval-r regexp-interval) 
		      (closed-right-p regexp-interval))
    (values (if match1 beg "") mid end (and match1 match2)))))
;;(split-string-3 "abcdef" (interval-co "b" "d"))

;;; split string regexp
(cl-defun split-string-once (string regexp) 
  "Splits STRING into the list (before-match match after-match)"
  (and (string-match regexp string)
       (split-at-position string (match-beginning 0) (match-end 0))))
;;(split-string-once "gabcdg" "\\`g")

(cl-defun split-string-regexp-1 (string regexp)
  (cl-loop for s = string then (third split)
	for split = (split-string-once s regexp)
	while split
	collect split))
(split-string-regexp-1 "babcdbefgb" "b")

(cl-defun split-string-regexp-list (string regexp &optional trim (side :both))
  "Splits STRING at places where REGEXP matches a substring of STRING.

It returns a list of strings \(substring-0 regexp-match-1
substring-1 regexp-match-2 substring-2 ... regexp-match-N
substring-N\).

If TRIM-REGEXP is non-nil, each string element is trimmed in
accordance with TRIM and SIDE. TRIM and SIDE correspond to the
REGEXP and SIDE arguments, respectively, in `split-string'."
  (let* ((split-data (split-string-regexp-1 string regexp))
	 (res (flatten (append (mapcar #'nbutlast (butlast split-data))
			       (last split-data)))))
    (if trim
      (cl-loop for x in res collect (string-trim* x trim side))
      res)))
;;(split-string-regexp-list "babc db efg b " "b" t)

(cl-defun split-string-regexp-pairs (string regexp
				     &key trim (side :both) (prefixed t))
  "Split STRING at places where REGEXP matches a substring of STRING.

It returns a list similiar to what `split-string-regexp-list'
does, but also groups the results to \(\(substring-0
regexp-match-1\) ... \(substring-N-1 regexp-match-N\)\) if
PREFIXED is nil and to \(\(substring-1 regexp-match-1\)
... \(substring-N regexp-match-N\)\) if it is true.

For the use of TRIM and SEPARATOR, see `split-string-regexp-list'"
  (cut (funcall (if prefixed #'rest #'butlast)
	 (split-string-regexp-list string regexp trim side))))
;;(split-string-regexp-pairs "babcdbefgb" "b" :trim nil :side :left)

(cl-defun split-string-regexp (string regexp
			       &optional omit-nulls trim (side :both))
  "Split STRING to a list of substrings at places where REGEXP
matches a substring of STRING.

If OMIT-NULLS is true, it detracts from the result all substrings
that are empty strings.

For the use of TRIM and SEPARATOR, see `split-string-regexp-list'."
  (let ((res (first
	      (nunzip (split-string-regexp-list string regexp trim side)))))
    (if omit-nulls (cl-delete "" res :test #'string=) res)))
;;(split-string-regexp "bab cd bqwerb" "b" t nil)

(cl-defun substring-intv (string regexp-interval &optional (cl-count 1))
  "Returns substring of STRING matching REGEXP-INTERVAL."
  (cl-assert (>= count 0))
  (multiple-value-call #'(lambda (beg mid end match) (if match mid ""))
		       (split-string-3 string regexp-interval)))

(cl-defun substring-intv (string regexp-interval &optional (n :once))
  "Returns substring of STRING matching REGEXP-INTERVAL
(see `split-string-3' for more details). If optional argument N
is :ONCE, the method returns either the first match or an empty
string. If N is :ALL it returns all matches in a list of strings.
If N is a non-negative it returns the the first N matches in a
list of strings."
  (let ((cl-count (cl-case n 
		 (:once 1)
		 (:all most-positive-fixnum)
		 (t n)))
	(res ()))
    (cl-assert (and (numberp count) (>= count 0)))
    (while (and (> (length string) 0)
		(> count 0))
      (cl-multiple-value-bind (beg mid end match)
	  (split-string-3 string regexp-interval)
	(if match
	  (push mid res)
	  (setq count 0))
	(setq string end))
      (cl-decf count))
    (if (eq n :once)
      (if (first res) (first res) "")
      (nreverse res))))
;(substring-intv "abcdefgabcdefg" (interval-oc "b" "d"))

(cl-defun string-replace-intv (string regexp-interval &optional (newstring ""))
  "Replaces all substrings in STRING that matches REGEXP-INTERVAL with
NEWSTRING (empty string by default)."
  (cl-multiple-value-bind (beg mid end match)
      (split-string-3 string regexp-interval)
    (if match 
      (concat beg newstring (string-replace-intv end regexp-interval newstring)) 
      string)))
;;(string-replace-intv "weqwesadqweqwe" (interval-cc "q" "w") "qu")

(cl-defun mb-string-replace (string regexp
			     &optional (newstring "") (preserve-case-p))
  "Replaces all substrings in STRING that matches REGEXP with
NEWSTRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newstring nil nil))
    (buffer-string)))
;;(mb-string-replace (setq string-replace-test-string "") "b" "c")

(cl-defun substitute-string (string subst-string from to)
  "Removes substring [FROM TO) from STRING and inserts
  SUBST-STRING at position FROM in STRING"
  (let ((substrings (split-at-position string from to)))
    (if (/= 3 (length substrings))
      (error "FROM (%d) and TO (%d) arguments are invalid for STRING %s"
	     string from to)
      (concat (first substrings) subst-string (third substrings)))))
;;(substitute-string "012qwe678" "ewq" 3 6)

(cl-defun string-replace-f (string regexp function)
  "Replaces all substrings in STRING that matches REGEXP with
result of FUNCTION."
  (let ((pos 0))
    (while pos
      (setq pos (string-match regexp string pos))
      (when pos
	;; Important to call MATCH-BEGINNING and MATCH-END before
	;; calling FUNCTION because of possibly side-effects in
	;; FUNCTION
	(let ((from (match-beginning 0))
	      (to (match-end 0))
	      (subst-string (funcall function string)))
	  (setq string (substitute-string string subst-string from to))
	  (cl-incf pos (length subst-string)))))
    string))

(cl-defun string-replace-map (string map)
  "Alters STRING according to MAP. A map is an list of pairs
\(FROM-STRING TO-STRING\). This is very slow"
  (dolist (x map string) 
    (setq string (mb-string-replace string (first x)
				 (if (listp (rest x))
				   (second x) ;cons is a one element list
				   (rest x)   ;cons is an element
				   )))))
:;(string-replace-map "Àøå" (list (cons "À" "Æ") (cons "ø" "Ø ")))
:;(string-replace-map "Àøå"
:;  (mapcar #'(lambda (x) (cons (first x) (third x))) *iso-latin1-encoding*))

(cl-defun string-indent-lines (string indent-string)
  "Prepend all lines in STRING with INDENT-STRING."
  (if (empty-string-p indent-string)
    string
    (apply #'concat
     (cons indent-string 
	   (infix-list (string-lines string)
		       (concat "\n" indent-string))))))
;;(string-indent-lines "a\nb\nc" "<<")


(cl-defun concat* (list &key (pre "") (in "") (infun nil) (suf "")
			  (test (constantly t))
			  (key #'identity)
			  (indent-string "")
			  discard-nil
			  discard-empty)
  "Concat strings in LIST adding prefix, suffix, and regular infixes.
If an element does not satisfy TEST then it is not inserted. Before
insertion, KEY modifies the elements. Note that KEY manipulation does
not affect TEST.
IN may also be a function (fn i), taking an index argument, see `infix-list'."
  (let* ((list* (mapcar key
		  (cl-remove-if-not test
		    (if discard-nil (cl-remove nil list) list)))))
    (string-indent-lines
     (concat pre
	     (apply #'concat
	       (infix-list (if discard-empty
			     (cl-remove "" list* :test #'string=) list*)
			   (or infun in) infun))
	     suf)
     indent-string)))

(cl-defun andcat (list &optional
		       (delimiter ", ")
		       (pair-delimiter " and ")
		       (last-delimiter (concat (string-trim delimiter)
					       pair-delimiter)))
  "Concatenate string list with commas and a final ', and '.
You may change the delimiters with the optional
arguments DELIMITER, LAST-DELIMITER, and PAIR-DELIMITER.

\(andcar ()\)
     ⇒ \"\"

\(andcar (\"Peter\")\)
     ⇒ \"Peter\"

\(andcar (\"Peter\" \"Paul\")\)
     ⇒ \"Peter and Paul\" 

\(andcar (\"Peter\" \"Paul\" \"Mary\")\)
     ⇒ \"Peter, Paul, and Mary\"
"
  (cl-case (length list)
    (0 "")
    (1 (car list))
    (2 (concat (first list) pair-delimiter (second list)))
    (t (concat* (butlast list)
	 :in delimiter
	 :suf (concat last-delimiter (last-elt list))))))
;;(andcat '("Peter" "Paul" "Mary"))

(cl-defun lines-to-string (lines)
  (concat* lines :in "\n"))
;;(lines-to-string '("first" "next" "last"))

(cl-defun quote-word () "Quotes closest word before point"
  (interactive "*")
  (backward-word 1) (insert "\"") 
  (forward-word 1) (insert "\""))

(cl-defun quote-region (beg end) "Quotes region"
  (interactive "*r")
  (goto-char beg) (insert "\"") 
  (goto-char (1+ end)) (insert "\""))

;(quote-word)

(define-key global-map "\M-\"" 'quote-word)
(define-key global-map [(control \")] 'quote-region); går ikke !!!

(defmacro string-case (arg &rest clauses)
  "Same as case, but compares with `string-equal' instead of `eql'."
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (cond ,@(mapcar #'(lambda (cl)
                           (let ((k (car cl)))
                             `(,(cond ((member k '(t otherwise))
                                       t)
                                      ((consp k)
                                       `(cl-member ,g ',k :test #'string=))
                                      (t `(string= ,g ',k)))
                               (progn ,@(cdr cl)))))
                       clauses)))))
;;(string-case "C" (("a" "b") t) (("a" "b" "C") 'qwe))

(def-edebug-spec string-case (form &rest ([&or (&rest form) form] form)))

(cl-defun region-to-string (&key (buffer (current-buffer)) start end)
  "Returns the current region content in BUFFER (default is current
buffer) as a string. The region can be overruled by keywords :START
and :END."
  (with-current-buffer buffer
    (buffer-substring (or start (point-min)) (or end (point-max)))))

(cl-defun region-to-lines (&key (buffer (current-buffer)) start end)
  "Returns the current region content in BUFFER (default is
current buffer) as a lines of string lines. The region can be
overruled by keywords :START and :END."
  (string-lines (region-to-string :buffer buffer :start start :end end)))

(cl-defun match-string* (num string &optional no-properties)
  "Same as `match-string', but if NO-PROPERTIES is non-nil,
`match-string-no-properties' is called instead."
  (funcall (if no-properties #'match-string-no-properties #'match-string)
    num string))

(cl-defun string-match* (regexp string &key (num 0) (start 0)
					 (return-null nil)
					 (subexpression-null nil)
					 (no-properties t))
  "Returns the part of STRING that matches REGEXP. NUM can also be a
tree. Then a tree of corresponding matches is returned."
  (if (string-match regexp string start)
    (rmapcar #'(lambda (num) (or (match-string* num string no-properties)
				 subexpression-null)) 
	     num)
    return-null))
;;(string-match* "\\(e\\)" "sdkjhalkqweee " :num '(1 0) :no-properties nil)
;;(string-match* "[[:alpha:]][[:alnum:]]*" "daystart")

(cl-defun string-matches-exact (regexp string &key
						(cl-count most-positive-fixnum)
						start (num 0) from-end)
  (with-temp-buffer
    (insert string)
    (goto-char (or start (if from-end (point-max) (point-min))))
    (cl-loop for i below count
	     for beg = (funcall (if from-end
				  #'re-search-backward
				  #'re-search-forward)
			 regexp nil t)
	     while beg
	     collect (buffer-substring (match-beginning num) (match-end num))
	     into res
	     finally return res)))
;;(string-matches-exact "\\(a.\\)b" "axbayb" :num 1 :count 123 :from-end t)

(cl-defun string-match-exact (regexp string &optional
					      (num 0) (start 0) (cl-count :once))
  "Returns the part of STRING that matches REGEXP."
  (and (string-match regexp string start)
       (string= (match-string num string) string)
       (match-string num string)))
;;(string-match-exact "..." "qwe")
;;(string-match-exact "q\\(qwe\\)?" "qqw")

(defvar *mb-trim-regexp* "[\n\t ]*") ;;(nilf *mb-trim-regexp*)
(defsubst mb-trim-regexp (regexp side)
  (format (cl-ecase side (:left "\\`%s") (:right "%s\\'"))
    (if (or (null regexp) (eql regexp t))
      *mb-trim-regexp* regexp)))
;;(mb-trim-regexp "a" :right)

;; Note! The following three functions can be in conflict with modules
;; that Slime loads. Should consider renaming them to mb-string-*
(cl-defun string-trim-left* (string &optional regexp)
  "Returns a copy of STRING trimmed at left with REGEXP."
  (or (third (split-string-once string (mb-trim-regexp regexp :left)))
      string))
;;(string-trim-left* "***qwe" "\\*")

(cl-defun string-trim-right* (string &optional regexp)
  "Returns a copy of STRING trimmed at right with REGEXP."
  (or (first (split-string-once string (mb-trim-regexp regexp :right)))
      string))
;;(string-trim-right* "qwe***" "\\**")

(cl-defun string-trim* (string &optional regexp (side :both))
  "Returns a copy of STRING trimmed with REGEXP at both sides.
If SIDE is :LEFT or :RIGHT, STRING is only trimmed at its left or
right side respectively."
  (cl-ecase side
    (:left (string-trim-left* string regexp))
    (:right (string-trim-right* string regexp))
    ((:both t) (string-trim-right* (string-trim-left* string regexp) regexp))))
;;(string-trim* " **qwe**" t)

(cl-defun string-remove-props (string)
  "Returns STRING without any emacs text props. For an entire buffer,
consider `buffer-substring-no-properties'"
  (message "This method is obsolete. Use `substring-no-properties' instead")
  (set-text-properties 0 (length string) nil string) string)

(cl-defun string-fill-paragraph (string)
  "Modyfies STRING as if it were a substring in a text buffer and
one applied `fill-paragraph' to it"
  (string-trim (with-temp-buffer
		 (text-mode)
		 (insert string)
		 (fill-paragraph nil)
		 (buffer-string))))
;;(string-fill-paragraph "asdf\n                 asdf\nesf")

(cl-defun substring* (string &optional (start 0) (end nil))
  "Same as SUBSTRING but takes negative limit arguments (meaning from end)"
  (let ((n (length string)))
    (substring string 
      (mod start n)
      (when (numberp end) (mod end n)))))
;;(substring* "012345" 0 -1)

(cl-defmacro with-lines ((line string) &rest body)
  `(cl-loop for ,line in (string-lines ,string)
	 do (progn ,@body)))
;;(let (lines) (with-lines (l "qwe\nqwe\nqwe") (push l lines)) lines)
(def-edebug-spec with-lines ((gate symbolp form) body))

(cl-defun multiply-string (s n)
  "TODO: Not too efficient"
  (if (< n 1)
    "" (apply #'concat (make-list n s))))
;;(mapcar (bind #'multiply-string "qwe" 1) (a-b -1 3))

(cl-defun integer-to-ordinal (n)
  (if (<= n 0)
    (error "Integer must be positive")
    (if (< 3 (mod n 100) 21)
      (format "%dth" n)
      (format "%d%s" n (cl-case (mod n 10) 
			 (1 "st") (2 "nd") (3 "rd") (t "th"))))))

(cl-defun integer-to-ordinal-string (n)
  (if (<= n 0)
    (error "Integer must be positive")
    (cl-case n 
      (1 "first") (2 "second") (3 "third") (4 "fourth") (5 "fifth")
      (6 "sixth") (7 "seventh") (8 "eighth") (9 "ninth")
      (20 "twentieth") (30 "thirtieth") (40 "fourtieth") (50 "fiftieth")
      (60 "sixtieth") (70 "seventieth") (80 "eightieth") (90 "ninetieth")
      (t (cond ((< 9 (mod n 100) 20)
		(format "%sth" (integer-to-literary-string n)))
	       ((< 20 (mod n 100) 100)
		(format "%s-%s" 
		  (integer-to-literary-string (interval-floor n 10))
		  (integer-to-ordinal-string (mod n 10))))
	       ((< 0 (mod n 10) 6)
		(format "%s %s" 
		  (integer-to-literary-string (interval-floor n 10))
		  (integer-to-ordinal-string (mod n 10)))))))))
;;(mapcar #'integer-to-ordinal-string '(14 20 21 100003))

(cl-defun integer-to-literary-string (n &key
					  ordinal-p
					  (short-separator "-")
					  (long-separator " "))
  (cond (ordinal-p 
	 (integer-to-ordinal-string n))
	((< n 0) 
	 (format "minus %d" (integer-to-literary-string (- n))))
	((< n 20)
	 (cl-case n
	   (0 "zero") (1 "one") (2 "two") (3 "three") (4 "four") (5 "five")
	   (6 "six") (7 "seven") (8 "eight") (9 "nine") (10 "ten") (11 "eleven")
	   (12 "twelve") (13 "thirteen") (14 "fourteen") (15 "fifteen")
	   (16 "sixteen")  (17 "seventeen")  (18 "eighteen")  (19 "nineteen") 
	   (100 "hundred") (1000 "thousand") (1000000 "million")
	   (1000000000 "billion")))
	((< n 100)
	 (format "%s%s"
	   (cl-case (interval-floor n 10)
	     (20 "twenty") (30 "thirty") (40 "forty") (50 "fifty")
	     (60 "sixty") (70 "seventy") (80 "eigthy") (90 "ninety"))
	   (if (zerop (mod n 10))
	     ""
	     (concat short-separator (integer-to-literary-string (mod n 10))))))
	((< n 1000)
	 (format "%s hundred%s"
	   (integer-to-literary-string (/ n 100))
	   (if (zerop (mod n 100))
	     ""
	     (concat long-separator (integer-to-literary-string (mod n 100))))))
	((< n 1000000)
	 (format "%s thousand%s"
	   (integer-to-literary-string (/ n 1000))
	   (if (zerop (mod n 1000))
	     ""
	     (concat long-separator (integer-to-literary-string (mod n 1000))))))
	((< n (expt 10 9))
	 (format "%s million%s"
	   (integer-to-literary-string (/ n 1000000))
	   (if (zerop (mod n 1000))
	     ""
	     (concat long-separator (integer-to-literary-string (mod n 1000))))))
	((< n (expt 10 12))
	 (format "%s billion"
	   (integer-to-literary-string (round (/ n 1000000000.0)))
	   (if (zerop (mod n 1000))
	     ""
	     (concat long-separator
		     (integer-to-literary-string (mod n 1000))))))
	(t (error "Number %d is not supported" n))))
(integer-to-literary-string 1234567890)

(defconst +password-special-characters+
  " !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"
  "A string consisting of all non-alphanumeric characters
commonly used in passwords")

(defconst +password-alphanum-characters+
  (cl-flet ((charexp (from-char to-char)
	      (cl-loop for c from from-char to to-char collect c)))
    (apply #'string (append (charexp ?0 ?9) (charexp ?A ?Z) (charexp ?a ?z))))
  "A string consisting of all alphanumeric characters.")

(defconst +password-all-characters+
  (concat +password-special-characters+ +password-alphanum-characters+)
  "A string consisting of all characters commonly used in passwords.")

(cl-defun random-string (length &optional
				(characters +password-alphanum-characters+)
				seed)
  "Returns a string of lenght LENGHT consisting of random characters.
The characters are randomly drawn from characters in the string
CHARACTERS. If this optional parameter is not provided
+PASSWORD-ALPHANUM-CHARACTERS+ is used as default. For optional
parameter SEED, see `random-integers'."
  (project characters (random-integers length 0 (length characters) seed)))
;;(random-string 8)

;;; Binary string game (BSG) from a Gad Saad video: To sides plays
;;; with a binary string, e.g. 010010. You make a move by removing
;;; either one of the end digits. The sides alternate making a single
;;; move. Eventually there must be one digit left. The side making
;;; this last, forced move wins iff the last digit is 1.
(cl-defun bsg-legal-p (string)
  (and (stringp string)
       (string-match "^[01]+$" string)))
;;(mapcar #'bsg-legal-p '(nil 1 "" "1" "0" "01" "10101" "101013"))

(cl-defun bsg-lost-p (string)
  "Return nil iff STRING is a theoretically won position.
A position is lost iff its perfect center substring is either 0,
11, or 010."
  (let ((n (length string)))
    (if (cl-oddp n)
      (or (string= (center string 1) "0")
	  (and (> n 1)
	       (string= (center string 3) "010")))
      (string= (center string 2) "11"))))
;;(mapcar #'bsg-lost-p '("0" "010" "11"))

(cl-defun bsg-move (string)
  "Make a move in the BSG game.
The result is a pair \(MOVE NEW-STRING\), where MOVE is
either :left or :right, and indicates from which end of STRING a
bit has been removed. NEW-STRING is the modified STRING after the
move has beed made.

If STRING contains only one bit the function only tells if
you (the other player) win or lose."
  (if (bsg-legal-p string)
    (if (= (length string) 1)
      (message "You %s!" (if (= (char string 0) ?0) "win" "lose"))
      (let ((left-move (substring string 1)))
       (if (bsg-lost-p left-move)
	 (list :left left-move)
	 (list :right (substring string 0 -1)))))
    (error "%s is not a legal BSG string!")))
;;(bsg-move "101")

(cl-defun format-integer-ranges (integers &optional sorted-p)
  "Return a string that expresses the list INTEGERS in a condensed form.
E.g. '(1 2 3 6 7 8 11) ==> \"1--3, 6--8, 11\".

If SORTED-P is not nil, the function assumes that INTEGERS are
sorted with #'<.

See also `group-consequtive-integers'."
  (concat* (group-consequtive-integers integers sorted-p)
    :in ", "
    :key #'(lambda (x)
	     (if (= (length x) 1)
	       (number-to-string (first x))
	       (concat* (list (first x) (last-elt x))
		 :in "--"
		 :key #'number-to-string)))))
;;(format-integer-ranges '(1 2 3 6 7 8 11))

(cl-defun read-whole-string (string)
  "Read the whole STRING as with `read' into a sexp list."
  (cl-loop with n = (length string)
	for (x . pos) = (read-from-string string pos)
	collect x while (< pos n)))
;;(read-whole-string "a b (c d)")

(cl-defun string-head (string &optional (n 1))
  (lines-to-string (head n (string-lines string))))

(cl-defun string-tail (string &optional (n 1))
  (lines-to-string (last (string-lines string) n)))
;;(string-tail "qwe\nqwe2\nqwe3")

(cl-defun alliterate-word (word)
  "Randomly scramble the letters in WORD, except the first and last."
  (if (> (length word) 3)
    (concat (substring word 0 1)
	    (randomize (substring word 1 -1))
	    (substring word -1))
    word))
;;(alliterate-word "qweriuiuie")

(cl-defun alliterate (string)
  "Randomly scramble the letter"
  (apply #'concat
    (mapcar #'alliterate-word
      (split-string-regexp-list string "[^[:alpha:]]"))))
;;(alliterate "Some are born great, some achieve greatness...")

(cl-defun alphanumerate (n &optional min-length (chars (a-b ?A ?Z)))
  "Convert N to string: 0 to A, 1 to B etc.
If MIN-LENGTH is greater than one, the default, convert 0 to AA, 1 to
AB, 25 to AZ, 26 to BA and so forth.

N can also be a list of integers. The function then converts each
of these integers to a string where all strings have the same
length."
  (let ((b (length chars)))
    (if (listp n)
      (cl-loop with l = (or min-length
			    (cl-loop for i in n maximize (uint-length i b)))
	       for i in n
	       collect (alphanumerate i l chars))
      (cl-coerce (mapcar (bind #'nth chars)
		(uint-to-n-base n b (or min-length 1)))
	      'string))))
;;(alphanumerate (a-b 0 26))

(cl-defun split-name (name)
  (cl-destructuring-bind (firsts last)
      (split-at-position (split-string name) -1)
    (list (concat* firsts :in " ") (car last))))
;;(split-name "Sverre Kristian Valskrå")

;;; tab format. TODO move this somewhere else
(cl-defun tab-column-type (column)
  (cond
    ((cl-every #'integerp column) 'integer)
    ((and (cl-every #'numberp column)
	  (cl-some #'floatp column)) 'float)
    ((cl-every #'stringp column) 'string)))
;;(tab-column-type '((1 2)))
;;(tab-column-type '("qe" "qe"))

(cl-defun tab-column-width (column &optional (type (tab-column-type column)))
  (awhen (cl-case type 
	   (string column)
	   (integer (mapcar #'number-to-string column)))
    (apply #'max (mapcar #'length it))))
;;(tab-column-width '(1 123))

(cl-defun tab-flag (width &optional type)
  (format "%%%s%ds" (if (eql type 'integer) "" "-") width))

(cl-defun tab-control-string (widths &key (type 'string) (separator " "))
  (let ((types (if (atom type) (make-list (length widths) type) type)))
    (concat* (cl-mapcar #'tab-flag widths types) :in separator)))
;;(tab-control-string '(4 5 1) :type '(integer integer string))

(cl-defun tab-format (string-table
		      &key header (column-separator " ") (underline-char ?=))
  (let ((first-row (first string-table)))
    (when header
      (cl-assert (and (= (length first-row) (length header))
		      (eql (tab-column-type header) 'string))))
    (let* ((columns (transpose string-table))
	   (types (mapcar #'tab-column-type columns))
	   (cwidths (cl-mapcar #'tab-column-width columns types))
	   (hwidths (and header (mapcar #'length header)))
	   (widths (if header (cl-mapcar #'max cwidths hwidths) cwidths))
	   (header (if header
		     (concat (apply #'format
			       (tab-control-string widths
				 :separator column-separator)
			       header)
			     "\n")
		     "")))
      (concat* string-table
	:pre (if underline-char
	       (format "%s%s\n"
		 header (make-string (length header) underline-char))
	       header)
	:key #'(lambda (x)
		 (apply #'format
		   (tab-control-string widths
		     :type types :separator column-separator)
		   x))
	:in "\n"))))
;;(tab-format '(("foobar" 1) ("qwe" 123456)) :header '("qwe" "ewq"))
;;(tab-format '((1 "xo") (123 "qwe")) :header '("n" "s") :column-separator "|")

(cl-defun uuid-regexp-1 (n)
  "Generate regexp for an UUID of length N."
  (format "[a-f0-9]\\{%s\\}" n))
;;(uuid-regexp-1 8)

(cl-defun uuid-regexp (&optional length separator)
  "Generate regexp for an UUID of length LEN.
If LEN is a list of integers it generates UUIDs of the
corresponding integer lengths and concatenates them with
SEPARATOR"
  (let ((len (or length '(8 4 4 4 12))))
    (if (integerp len)
      (uuid-regexp-1 len)
      (concat* (mapcar #'uuid-regexp-1 len) :in (or separator "-")))))
;;(uuid-regexp '(8 4 4 4 12) "-")
;;(string-to-integer "1091864")

(cl-defun uuid-p (string)
  (or (string-match (uuid-regexp 24) string)
      (string-match (uuid-regexp '(8 4 4 4 12) "-") string)))
;;(uuid-p "53bf7368-f985-4061-9283-b3065a578a7f")
;;(length "b3065a578a7f")
;;(uuid-p "594d3459cb99b500132bc742")

(cl-defun commented-string-p (string comment-start &optional empty-is-comment-p)
  (or (and empty-is-comment-p (empty-string-p string))
      (string-match (format "^%s" comment-start) string)))
;;(commented-string-p "#!/bin/bash" "#")
;;(commented-string-p "" "#" t)

(cl-defun sort-strings-with-comments (strings comment-start)
  (let ((comments ()))
    (cl-loop for string in strings
	     if (commented-string-p string comment-start t)
	     do (push string comments)
	     else
	     collect (list (nreverse comments) string) into res
	     and do (setf comments ())
	     finally return (flatten (cl-sort res #'string< :key #'second)))))
;;(sort-strings-with-comments (file-lines "~/git/utils/bin/adafind.sh") "#")

(defconst +supported-alphabet-languages+ '(:en :no))

(cl-defun alphabet (&optional (language :en))
  "Return a string of characters in LANGUAGE sorted alphabetically.
Default language is :en. Other supported languages are: :no"
  (cl-case language
    (:en "abcdefghijklmnopqrstuvwxyz")
    (:no "abcdefghijklmnopqrstuvwxyzæøå")
    (otherwise (error "Unknown language %S! Supported languages are: "
		      +supported-alphabet-languages+))))

(provide 'mb-utils-strings)
