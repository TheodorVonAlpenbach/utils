(require 'cl)
(require 'mb-utils-div)
(require 'mb-utils-sets)
;;faculty

(defun string> (string1 string2)
  "Returns non-nil iif string1 > string2. See `string<'"
  (string< string2 string1))

(defun string<= (string1 string2)
  "Returns non-nil iif string1 > string2. See `string<'"
  (not (string> string1 string2)))

(defun string>= (string1 string2)
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

(defun lower-case-p (character)
  "Common Lisp function. Checks if CHARACTER is invariant under
case-table."
  (= (aref (current-case-table) character) character))
;;(lower-case-p ?Æ) => nil

(defun upper-case-p (character)
  "Inverse of `lower-case-p'."
  (not (lower-case-p character)))

(defun integer-string-p (string)
  (and (stringp string)
       (string-match-exact "[0-9]+" string)))
;;(mapcar #'integer-string-p '("123" 123 "a"))

(defun integer-to-string (string)
  "Similar to `number-to-string', but returns nil (not 0) if the
STRING is not an integer, otherwise returns the resulting integer."
  (and (integer-string-p string)
       (string-to-number string)))
;;(mapcar #'integer-to-string '("123" 123 "a"))

(defmacro char (string index)
  "Common Lisp CHAR"
  `(aref ,string ,index))

(defun char< (char &rest chars) (apply #'-< char chars))
(defun char= (char &rest chars) (apply #'-= char chars))
(defun char/= (char &rest chars) (apply #'/= char chars))
;;(char/= ?a ?b ?a)

(defun string-to-character-list (string) (coerce string 'list))
;;(string-to-character-list "abc")

(defun copy-case (string pattern)
  "Returns copy of STRING that has similar case as PATTERN: If PATTERN
starts with two upper case characters, the entire result is upcased. If
PATTERN starts with an upcase character followed by a downcase, the
result is capitalized. Else the entire result is downcased."
  (if (upper-case-p (char pattern 0))
    (if (upper-case-p (char pattern 1))
      (upcase string)
      (capitalize string))
    (downcase string)))

(defun pure-string (string)
  "Removes formatting from STRING and returns result."
  (typecase string
    (sequence (elt string 0))
    (string string)
    (otherwise (error "unknown string format"))))

(defun empty-string-p (string)
  (and (stringp string)
       (zerop (length string))))
;;(empty-string-p "")

(defun not-empty (string)
  "Returns 'nil if STRING is empty. Otherwise returns STRING. Useful
in connection with e.g. `or'"
  (and (not (empty-string-p string))
       string))
;(not-empty "")

(defun string-match-end (regexp string &optional start)
  "Same as `string-match', except that it returns the position after
match."
  (let ((pos (string-match regexp string)))
    (if pos (match-end 0) nil)))

(defun split-string-at-pos (string &rest positions)
  "Splits STRING at POSITIONS and return the resulting substrings as a
list of strings"
  (loop for position in (nreverse (cons (length string) (nreverse positions)))
	  for start = 0 then end
	for end = position
	collect (substring string start end)))
;;(split-string-at-pos "qweqwe" 2)

(cl-defun split-string-by-length (string n &key (from-end nil))
  "Splits STRING to substrings of length N.
Only the length of the last substring (or the first substring, if
from-end is t) is less than (or equal to) N."
  (let ((split-positions (a-b (if from-end (mod (length string) n) n)
			      (length string) n)))
    (remove "" (apply #'split-string-at-pos string split-positions))))
;;(split-string-by-length "0123456789012345" 3 :from-end t)

(defun regexp-consecutive-matches (regexp string)
  "Returns the position boundaries of the consecutive substrings of
STRING that matches REGEXP"
  (loop with position = 0
	while (string-match regexp string position)
	do (setq position (match-end 0))
	collect (list (match-beginning 0) (match-end 0))))
;;(regexp-consecutive-matches "a" "aasdfaa")

(defun split-string-modify-positions (positions with-separator)
  (case with-separator
    ((:none nil) (cut (cons 0 (flatten positions)) 2 t))
    (:left (cut (cons 0 (flatten (mapcar #'(lambda (x) (list (first x) (first x))) positions))) 2 t))
    (:right (cut (cons 0 (flatten (mapcar #'(lambda (x) (list (second x) (second x))) positions))) 2 t))
    (:only positions)))
;;(split-string-modify-positions '((1 2) (5 6)) :none)

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
;;(split-string* "09:16  Text" "[0-2][0-9]:[0-5][0-9]" :right)

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
      (values (first res-first) (second res-second) (third res-first) (third res-second)))))
;;(split-string-2 "abcdef" "g" nil t)

(cl-defun split-string-3 (string regexp-interval)
  "Splits STRING into (BEG MID END), where MIN is first match of
REGEXP-INTERVAL in STRING. If the regexps in REGEXP-INTERVAL overlap,
the result is undefined."
  (multiple-value-bind
  (beg rest match1)
  (split-string-2 string
		  (interval-l regexp-interval)
		  (open-left-p regexp-interval))
  (multiple-value-bind 
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
       (split-string-at-pos string (match-beginning 0) (match-end 0))))
;;(split-string-once "gabcdg" "\\`g")

(defun split-string-regexp-1 (string regexp)
  (loop for s = string then (third split)
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
      (loop for x in res collect (string-trim x trim side))
      res)))
;;(split-string-regexp-list "babc db efg b " "b" t)

(cl-defun split-string-regexp-pairs (string regexp &key trim (side :both) (prefixed t))
  "Splits STRING at places where REGEXP matches a substring of STRING.

It returns a list similiar to what `split-string-regexp-list'
does, but also groups the results to \(\(substring-0
regexp-match-1\) ... \(substring-N-1 regexp-match-N\)\) if
PREFIXED is nil and to \(\(substring-1 regexp-match-1\)
... \(substring-N regexp-match-N\)\) if it is true.

For the use of TRIM and SEPARATOR, see `split-string-regexp-list'"
  (cut (funcall (if prefixed #'rest #'butlast)
		(split-string-regexp-list string regexp trim side))))
;;(split-string-regexp-pairs "babcdbefgb" "b" :trim nil :side :left)

(cl-defun split-string-regexp (string regexp &optional omit-nulls trim (side :both))
  "Splits STRING to a list of substrings at places where REGEXP matches a substring of STRING.

If OMIT-NULLS is true, it detracts from the result all substrings
that are empty strings.

For the use of TRIM and SEPARATOR, see `split-string-regexp-list'."
  (let ((res (first (nunzip (split-string-regexp-list string regexp trim side)))))
    (if omit-nulls (cl-delete "" res :test #'string=) res)))
;;(split-string-regexp "bab cd bqwerb" "b" t nil :right)

(defun substring-intv (string regexp-interval &optional (count 1))
  "Returns substring of STRING matching REGEXP-INTERVAL."
  (assert (>= count 0))
  (multiple-value-call #'(lambda (beg mid end match) (if match mid ""))
		       (split-string-3 string regexp-interval)))

(cl-defun substring-intv (string regexp-interval &optional (n :once))
  "Returns substring of STRING matching REGEXP-INTERVAL (see `split-string-3' for more details).
If optional argument N is :ONCE, the method returns either the
first match or an empty string. If N is :ALL it returns all
matches in a list of strings. If N is a non-negative it returns
the the first N matches in a list of strings."
  (let ((count (case n 
		 (:once 1)
		 (:all most-positive-fixnum)
		 (t n)))
	(res ()))
    (assert (and (numberp count) (>= count 0)))
    (while (and (> (length string) 0)
		(> count 0))
	  (multiple-value-bind (beg mid end match) (split-string-3 string regexp-interval)
	    (if match
	      (push mid res)
	      (setq count 0))
	    (setq string end))
	  (decf count))
    (if (eq n :once)
      (if (first res) (first res) "")
      (nreverse res))))
;(substring-intv "abcdefgabcdefg" (interval-oc "b" "d"))

(cl-defun string-replace-intv (string regexp-interval &optional (newstring ""))
  "Replaces all substrings in STRING that matches REGEXP-INTERVAL with
NEWSTRING (empty string by default)."
  (multiple-value-bind (beg mid end match) (split-string-3 string regexp-interval)
    (if match 
      (concat beg newstring (string-replace-intv end regexp-interval newstring)) 
      string)))
;;(string-replace-intv "weqwesadqweqwe" (interval-cc "q" "w") "qu")

(cl-defun string-replace (string regexp &optional (newstring "") (preserve-case-p))
  "Replaces all substrings in STRING that matches REGEXP with
NEWSTRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newstring nil nil))
    (buffer-string)))
;(string-replace (setq string-replace-test-string "ababab") "b" "c")

(defun substitute-string (string subst-string from to)
  "Removes substring [FROM TO) from STRING and inserts
  SUBST-STRING at position FROM in STRING"
  (let ((substrings (split-string-at-pos string from to)))
    (if (/= 3 (length substrings))
      (error "FROM (%d) and TO (%d) arguments are invalid for STRING %s" string from to)
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
	  (incf pos (length subst-string)))))
    string))
;;(string-replace-f "-qwewe-" "\\(qw\\)e" #'(lambda (string) (upcase (match-string 1 string))))

(cl-defun string-replace-map (string map)
  "Alters STRING according to MAP. A map is an alist of '(FROM-STRING .
TO-STRING)-s. This is very slow"
  (dolist (x map string) 
      (setq string (string-replace string (first x) (if (listp (rest x))
						      (second x) ;cons is a one element list
						      (rest x)   ;cons is an element
						      )))))
; (string-replace-map "Àøå" (list (cons "À" "Æ")))
; (string-replace-map "Àøå"
;   (mapcar #'(lambda (x) (cons (first x) (third x))) *iso-latin1-encoding*))

(defun infix-list (lst infix &optional is-function discard-empty-string) "'(a b c) => '(a INFIX b INFIX c)"
  "INFIX may be a function (FN I), where argument I is the nth time FN will be called by INFIX-LIST"
  (if lst
    (let ((rlst (cons (first lst) nil)))
      (dolist (i (rest lst) rlst)
	(setf rlst (append rlst (list (if is-function
					(funcall infix i)
					infix) 
				      i)))))))

(defun infix-list (list infix &optional is-function) 
  "'(a b c) => '(a INFIX b INFIX c)"
  "INFIX may be a function (FN I), where argument I is the nth
time FN will be called by INFIX-LIST"
  (if list
    (cons (first list)
	  (loop for x in (rest list)
		for i from 0
		collect (if is-function 
			  (funcall infix i) infix)
		collect x))))
;(infix-list '(a b c) #'1+ t)

(defun string-indent-lines (string indent-string)
  (apply #'concat
	 (cons indent-string 
	       (infix-list (string-to-lines string)
			   (concat "\n" indent-string)))))
;;(string-indent-lines "a\nb" "  ")

(cl-defun concat* (list &key (pre "") (in "") (infun nil) (suf "")
			(test (constantly t)) (key #'identity)
			(indent-string "") discard-nil)
  "Concat strings in LIST adding prefix, suffix, and regular infixes.
If not an element satisfies TEST then it is not inserted. Before
insertion, KEY modifies the elements. Note that KEY manipulation does
not affect TEST.
IN may also be a function (fn i), taking an index argument, see `infix-list'."
  (let* ((list* (remove-if-not test (if discard-nil (remove nil list) list)))
	 (list** (infix-list (mapcar key list*) (or infun in) infun))
	 (res (concat pre (apply #'concat list**) suf)))
    (if (empty-string-p indent-string)
      res
      (string-indent-lines res indent-string))))
;(concat* '("1" "2" "3") :in "\n" :indent-string ">>")
;(concat* '("a" "b" "c") :pre "(" :in " " :suf ")" :test #'(lambda (x) (string= x "a")) :key #'length)
;(concat* '(1 2 nil 3) :test #'oddp :key #'int-to-string :discard-nil t)
;(concat* '((1 (2)) (1 (3))) :test #'second :key #'first)
;(concat* '(("ee") ("d3" "d1") ("4d") ("ef") ("ag5")) :pre "\\([^" :suf "]" :key (lambda (x) (char-to-string (first x))))
;(concat* '("1" "2" "3") :in "\n" :indent-string ">>")
;;(concat* '("" "" "3") :in "\n" :indent-string ">>")

(defun string-to-lines (string &optional remove-empty-p)
  "Returns a list of strings, each string being line substring in
STRING."
  (if remove-empty-p
    (cl-remove "" (split-string string "\n") :test #'string=)
    (split-string string "\n")))
;;(string-to-lines "a\nb\n\nc\n")

(defalias 'string-lines 'string-to-lines)
;;(string-lines "a\nb\n\nc\n")

(defun quote-word () "Quotes closest word before point"
  (interactive "*")
  (backward-word 1) (insert "\"") 
  (forward-word 1) (insert "\""))

(defun quote-region (beg end) "Quotes region"
  (interactive "*r")
  (goto-char beg) (insert "\"") 
  (goto-char (1+ end)) (insert "\""))

;(quote-word)

(define-key global-map "\M-\"" 'quote-word)
(define-key global-map [(control \")] 'quote-region); går ikke !!!

(defmacro string-case (arg &rest clauses)
  "Same as CASE, but compares with STRING-EQUAL instead of EQL."
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
  (string-to-lines (region-to-string :buffer buffer :start start :end end)))

(defun match-string* (num string &optional no-properties)
  "Same as `match-string', but if NO-PROPERTIES is non-nil,
`match-string-no-properties' is called instead."
  (funcall (if no-properties #'match-string-no-properties #'match-string) num string))

(cl-defun string-match* (regexp string &key (num 0) (start 0) (return-null nil) (subexpression-null nil) (no-properties t))
  "Returns the part of STRING that matches REGEXP. NUM can also be a
tree. Then a tree of corresponding matches is returned."
  (if (string-match regexp string start)
    (rmapcar #'(lambda (num) (or (match-string* num string no-properties)
				 subexpression-null)) 
	     num)
    return-null))
;;(string-match* "\\(e\\)" "sdkjhalkqweee " :num '(1 0) :no-properties nil)
;;(string-match* "[[:alpha:]][[:alnum:]]*" "daystart")

(cl-defun string-matches-exact (regexp string &key (count most-positive-fixnum) start (num 0) from-end)
  (with-temp-buffer
    (insert string)
    (goto-char (or start (if from-end (point-max) (point-min))))
    (loop for i below count
	  for beg = (funcall (if from-end #'re-search-backward #'re-search-forward) regexp nil t)
	  while beg collect (buffer-substring (match-beginning num) (match-end num)) into res
	  finally return res)))
;;(string-matches-exact "\\(a.\\)b" "axbayb" :num 1 :count 123 :from-end t) --> ("ayb" "axb")

(cl-defun string-match-exact (regexp string &optional (num 0) (start 0) (count :once))
  "Returns the part of STRING that matches REGEXP."
  (and (string-match regexp string start)
       (string= (match-string num string) string)
       (match-string num string)))
;;(string-match-exact "..." "qwe")
;;(string-match-exact "q\\(qwe\\)?" "qqw")

(defvar *mb-trim-regexp* "[\n\t ]*") ;;(nilf *mb-trim-regexp*)
(defsubst mb-trim-regexp (regexp side)
  (format (ecase side (:left "\\`%s") (:right "%s\\'"))
    (if (or (null regexp) (eql regexp t))
      *mb-trim-regexp* regexp)))
;;(mb-trim-regexp "a" :right)

(defun string-trim-left (string &optional regexp)
  "Returns a copy of STRING trimmed at left with REGEXP."
  (or (third (split-string-once string (mb-trim-regexp regexp :left)))
      string))
;;(string-trim-left "***qwe" "\\*")

(defun string-trim-right (string &optional regexp)
  "Returns a copy of STRING trimmed at right with REGEXP."
  (or (first (split-string-once string (mb-trim-regexp regexp :right)))
      string))
;;(string-trim-right "qwe***" "\\**")

(cl-defun string-trim (string &optional regexp (side :both))
  "Returns a copy of STRING trimmed with REGEXP at both sides.
If SIDE is :LEFT or :RIGHT, STRING is only trimmed at its left or
right side respectively."
  (ecase side
    (:left (string-trim-left string regexp))
    (:right (string-trim-right string regexp))
    ((:both t) (string-trim-right (string-trim-left string regexp) regexp))))
;;(string-trim " **qwe**" t)

(defun string-remove-props (string)
  "Returns STRING without any emacs text props. For an entire buffer,
consider `buffer-substring-no-properties'"
  (message "This method is obsolete. Use `substring-no-properties' instead")
  (set-text-properties 0 (length string) nil string) string)

(defun string-fill-paragraph (string)
  "Modyfies STRING as if it were a substring in a text buffer and one applied `fill-paragraph' to it"
  (string-trim (with-temp-buffer
		 (text-mode)
		 (insert string)
		 (fill-paragraph nil)
		 (buffer-string))))
;;(string-fill-paragraph "asdf\n                 asdf\nesf")

;;(let ((string #("http://www.aftenposten.no/nyheter/siste100/" 0 43 (fontified nil)))) (string-remove-props string) string)

(cl-defun substring* (string &optional (start 0) (end nil))
  "Same as SUBSTRING but takes negative limit arguments (meaning from end)"
  (let ((n (length string)))
    (substring string 
      (mod start n)
      (when (numberp end) (mod end n)))))
;;(substring* "012345" 0 -1)

(cl-defmacro with-lines ((line string) &rest body)
  `(loop for ,line in (string-to-lines ,string)
	 do (progn ,@body)))
;;(let (lines) (with-lines (l "qwe\nqwe\nqwe") (push l lines)) lines)

(defun multiply-string (s n)
  "TODO: Not too efficient"
  (if (< n 1)
    "" (apply #'concat (make-list n s))))
;;(mapcar (bind #'multiply-string "qwe" 1) (a-b -1 3))

(defun integer-to-ordinal (n)
  (if (<= n 0)
    (error "Integer must be positive")
    (if (between (mod n 100) 3 21)
      (format "%dth" n)
      (format "%d%s" n (case (mod n 10) 
			 (1 "st") (2 "nd") (3 "rd") (t "th"))))))
;;(prin1 (mapcar #'integer-to-ordinal '(1 2 3 4 9 10 11 12 13 14 20 21 22 23 24 100003)))

(defun integer-to-ordinal-string (n)
  (if (<= n 0)
    (error "Integer must be positive")
    (case n 
      (1 "first") (2 "second") (3 "third") (4 "fourth") (5 "fifth")
      (6 "sixth") (7 "seventh") (8 "eighth") (9 "ninth")
      (20 "twentieth") (30 "thirtieth") (40 "fourtieth") (50 "fiftieth")
      (60 "sixtieth") (70 "seventieth") (80 "eightieth") (90 "ninetieth")
      (t (cond ((between (mod n 100) 9 20)
		(format "%sth" (integer-to-literary-string n)))
	       ((between (mod n 100) 20 100)
		(format "%s-%s" 
		  (integer-to-literary-string (interval-floor n 10))
		  (integer-to-ordinal-string (mod n 10))))
	       ((between (mod n 10) 0 6)
		(format "%s %s" 
		  (integer-to-literary-string (interval-floor n 10))
		  (integer-to-ordinal-string (mod n 10)))))))))
;;(prin1 (mapcar #'integer-to-ordinal-string '(1 2 3 4 9 10 11 12 13 14 20 21 22 23 24 100003)))
;;(mapcar #'integer-to-ordinal-string '( 14 20 21 100003))

(defun integer-to-literary-string (n &optional ordinal-p)
  (cond (ordinal-p 
	 (error "Ordinals not supported"))
	((< n 0) 
	 (format "minus %d" (integer-to-literary-string (- n))))
	((< n 20)
	 (case n
	   (0 "zero") (1 "one") (2 "two") (3 "three") (4 "four") (5 "five") (6 "six") (7 "seven") (8 "eight") (9 "nine") 
	   (10 "ten") (11 "eleven") (12 "twelve") (13 "thirteen") (14 "fourteen") (15 "fifteen")  (16 "sixteen")  (17 "seventeen")  (18 "eighteen")  (19 "nineteen") 
	   (100 "hundred") (1000 "thousand") (1000000 "million") (1000000000 "billion")))
	((< n 100)
	 (format "%s%s"
	   (case (interval-floor n 10)
	     (20 "twenty") (30 "thirty") (40 "forty") (50 "fifty") (60 "sixty") (70 "seventy") (80 "eigthy") (90 "ninety"))
	   (if (zerop (mod n 10)) "" (concat "-" (integer-to-literary-string (mod n 10))))))
	((< n 1000)
	 (format "%s hundred%s"
	   (integer-to-literary-string (/ n 100))
	   (if (zerop (mod n 100)) "" (concat " " (integer-to-literary-string (mod n 100))))))
	((< n 1000000)
	 (format "%s thousand%s"
	   (integer-to-literary-string (/ n 1000))
	   (if (zerop (mod n 1000)) "" (concat " " (integer-to-literary-string (mod n 1000))))))
	((< n 10E9)
	 (format "%s million%s"
	   (integer-to-literary-string (/ n 1000000))
	   (if (zerop (mod n 1000)) "" (concat " " (integer-to-literary-string (mod n 1000))))))
	((< n 10E12)
	 (format "%s billion"
	   (integer-to-literary-string (/ n 1000000000.0))
	   (if (zerop (mod n 1000)) "" (concat " " (integer-to-literary-string (mod n 1000))))))))
;;(mapcar (bind #'integer-to-literary-string nil) (list 21 99 100 100003 most-positive-fixnum))

(provide 'mb-utils-strings)
