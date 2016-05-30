(defun region-beginning* (&optional force)
  "See `region-beginning' and `mark'"
  (min (point) (mark force)))

(defun region-end* (&optional force)
  "See `region-end' and `mark'"
  (max (point) (mark force)))
;;(region-end*)

(cl-defun marked-text-region (&optional (buffer (current-buffer)))
  "Returns nil if mark is not active, else it returns the marked text.
The function operates on BUFFER which defaults to the current
pbuffer. The marked text is returned without text properties iff
WITH-PROPERTIES is nil."
  (and mark-active
       (with-buffer buffer
	 (list (region-beginning* t) (region-end*)))))
;;(marked-text-region)

(cl-defun marked-text (&optional with-properties (buffer (current-buffer)))
  "Returns nil if mark is not active, else it returns the marked text.
The function operates on BUFFER which defaults to the current
buffer. The marked text is returned without text properties iff
WITH-PROPERTIES is nil."
  (awhen (marked-text-region buffer)
    (apply (if with-properties #'buffer-substring #'buffer-substring-no-properties) it)))
;;(marked-text)

(cl-defun buffer-clear (&optional (buffer (current-buffer)))
  (kill-region (point-min) (point-max)))

(cl-defmacro buffer-do-regions (beg end (regexp &optional (subexp 0)) &rest body)
  "With current buffer. Start at min point and traverse buffer in
regions defined by REGEXP. For each region, BEG and END are set to the
end point that defines the region, before the BODY forms are
executed. No validation checks in this version."
  (with-gensyms (gregexp gsubexp)
    `(let ((,gregexp ,regexp)
	   (,gsubexp ,subexp))
       (save-excursion
	 (goto-char (point-min))
	 (while (re-search-forward ,gregexp nil t)
	   (setq ,beg (match-beginning ,gsubexp))
	   (setq ,end (match-end ,gsubexp))
	   ,@body)))))
;;(buffer-do-regions a b ("^(def\\([^[:space:]]* \\) 1") (message (buffer-substring a b))) => un, un*, macro*

(cl-defun buffer-tail (&optional (n 10) (buffer (current-buffer)))
  "Returns a string containing the N last lines in buffer"
  (with-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (previous-line n)
      (buffer-substring-no-properties (point) (point-max)))))

(defun buffer-line-point (line)
  "Returns point at beginning of the current buffer's n'th LINE.
  If LINE is negative, it is interpreted as the n'th last LINE in
  buffer. For large buffers this is also a faster way to identify
  the point. Note that 0 returns beginning of first line, -1
  means beginning of last line. 

  TODO: some magic with trailing newline. Also it is a bit
  clumsily written, why do I have to use beginning of line?"
  (save-excursion
    (cond ((< line 0) 
	       (goto-char (point-max))
	       (previous-line (- line))
	       (beginning-of-line))
	  (t
	   (goto-char (point-min))
	   (next-line line)
	   (beginning-of-line)))
    (point)))
;;(buffer-line-point 1)

(defun buffer-paragraph-point (line)
  "Returns point at beginning of the current buffer's n'th PARAGRAPH.
  If PARAGRAPH is negative, it is interpreted as the n'th last PARAGRAPH in
  buffer. For large buffers this is also a faster way to identify
  the point. Note that 0 returns beginning of first paragraph, -1
  means beginning of last paragraph.
TODO: some magic with trailing newline."
  (let ((old-point (point))
	(result (cond ((< line 0) 
		       (goto-char (point-max))
		       (backward-paragraph (- line)))
		      (t
		       (goto-char (point-min))
		       (forward-paragraph line)))))
    (goto-char old-point)
    result))

(cl-defun buffer-substring*-old (&key from-line to-line from-point to-point from-paragraph to-paragraph with-properties)
  "Generalized version of buffer-substring-no-properties.
  Delimiters are set using either line, point or paragraph. These
  entities are treated exclusive, so, for instance, if TO-LINE is
  set, then neither TO-POINT nor TO-PARAGRAPH should be set for
  clarity. However, it is indeed allowed, but the entity actually
  chosen by the method is the first set of line, point,
  paragraph.

  The from and to parts may take different entities. For
  instance, setting to-line and from-point is totally legal.

  The values are integers, which may be negative. If negative, it
  is counted backwards from end of buffer. This is also a hint to
  the algorithm to search from end, which for large buffers is
  faster.

  If no from entity is set, the from point is point-min. If no to
  entity is set, the to point is set to point-max."
  (let ((a (or (and from-line (buffer-line-point from-line))
	       from-point
	       (and from-paragraph (buffer-paragraph-point from-paragraph))
	       (point-min)))
	(b (or (and to-line (buffer-line-point to-line))
	       to-point
	       (and to-paragraph (buffer-paragraph-point to-paragraph))
	       (point-max))))
    (if with-properties
      (buffer-substring a b)
      (buffer-substring-no-properties a b))))

(cl-defun rpoint (&optional (point (point)))
  "Same as `point' but accepts negative argument POINT. If POINT
is -1, the last point in buffer is returned, if -2 the last but
one point and so on."
  (mod point (1+ (point-max))))
;;(goto-char (rpoint -2))

(cl-defun buffer-substring* (&key (start (point-min))
				  length
				  (end (if length (+ start length) (point-max)))
				  (unit :point)
				  with-properties)
  "Generalized version of `buffer-substring' and `buffer-substring-no-properties'.
Delimiters START and END are set in relation to UNIT, which may
currently be either :point, :line or :paragraph.

START and END are integers, which may be negative. If the integer
is negative, it is interpreted as modulo the total number of
units in buffer. This is also a hint to the algorithm to search
from end, which for large buffers is faster.

If no from entity is set, the from point is point-min. If no to
entity is set, the to point is set to point-max.

Note that line numbers and paragraph numbers (check) starts from base 0."
  (let ((a (case unit
	     (:point (rpoint start))
	     (:line (buffer-line-point start))
	     (:paragraph (buffer-paragraph-point start))))
	(b (if end
	     (case unit
	       (:point (rpoint end))
	       (:line (buffer-line-point end))
	       (:paragraph (buffer-paragraph-point end)))
	     (point-max))))
    (if with-properties
      (buffer-substring a b)
      (buffer-substring-no-properties a b))))
;;(buffer-substring* :start (point) :length 1 :unit :line)
;;(point-min)

(cl-defun insert-at (thing &optional (point (point)))
  "`insert's THING at POINT."
  (save-excursion
    (goto-char point)
    (insert thing)))

(defun next-line-point () "Return position of point after nextline"
  (save-excursion
    (next-line 1)
    (point)))

(defalias 'region-string 'buffer-substring-no-properties)
;;(region-string 1 100)

(defun region-lines (beg end)
  "Returns a list of strings, each string being line in region"
  (interactive "r")
  (string-to-lines (region-string beg end)))

(defun region-as-line-strings (beg end)
  (interactive "r")
  (warn "Deprecated. Use REGION-LINES instead."))

(defsubst bob ()
  "Move POINT to beginning of buffer and return its value"
  (goto-char (point-min)))
(defsubst eob ()
  "Move POINT to end of buffer and return its value"
  (goto-char (point-max)))

(defsubst bop ()
  "Move POINT to beginning of paragraph and return its value"
  (backward-paragraph 1)
  (when (blank-line-p)
    (forward-char 1))
  (point))
;;(bop)

(cl-defun buffer-lines (&optional (buffer (current-buffer)))
  (with-buffer buffer (save-excursion (region-lines (bob) (eob)))))
;;(buffer-lines)

(defsubst eop ()
  "Move POINT to end of paragraph and return its value"
  (forward-paragraph 1)
  (point))
;;(eop)

(defun paragraph-region ()
  "Return the region of the current paragraph.
The return value is a pair of points \(START END\)."
  (save-excursion
    (list (bop) (eop))))
;;(paragraph-region)

(defun paragraph-string ()
  "Return the paragraph around POINT as a string."
  (save-excursion
    (buffer-substring-no-properties (bop) (eop))))
;;(paragraph-string)

(defun current-paragraph-as-string () (warn "Deprecated. Use PARAGRAPH-STRING instead.") (paragraph-string))

(cl-defun bol (&key linum (offset 0) restrict-to-current-field)
  "Move point to the beginning of the current line and return its value.
If RESTRICT-TO-CURRENT-FIELD is true, the function constrains
point to the current field, see `beginning-of-line'"
  (let ((inhibit-field-text-motion (not restrict-to-current-field)))
    (when linum (goto-line linum))
    (beginning-of-line (1+ offset))
    (point)))
;;(bol :linum nil)

(cl-defun eol (&key linum (offset 0) restrict-to-current-field)
  "Move point to the end of the current line and returns its value.
For RESTRICT-TO-CURRENT-FIELD, see `bol'."
  (let ((inhibit-field-text-motion (not restrict-to-current-field)))
    (when linum (goto-line linum))
    (end-of-line (1+ offset))
    (point)))
;;(save-excursion (list (bol) (eol) (bob) (eob)))

(cl-defun line-region (&optional linums (buffer (current-buffer)))
  "Returns the region covering the current line in BUFFER.
If LINUMS is specified it returns the region covering those
lines."
  (with-buffer buffer
    (list (bol :linum (and linums (reduce #'min (listify linums))))
	  (eol :linum (and linums (reduce #'max (listify linums)))))))
;;(line-as-region)

(cl-defun line-string (&key to-point from-point restrict-to-current-field with-properties (point (point)))
  "Return a string of the line containing point.
If TO-POINT is true, only the part of line before `point' is
returned. If FROM-POINT is true, only the part of line after
`point' is returned. If WITH-PROPERTIES is true, properties are
included in the returned string. For RESTRICT-TO-CURRENT-FIELD,
see `bol'"
  (assert (nand to-point from-point)
	  "Both to-point and from-point cannot be non-nil!")
  (save-excursion
    (let* ((end (eol :restrict-to-current-field restrict-to-current-field))
	   (beg (bol :restrict-to-current-field restrict-to-current-field)))
      (cond (to-point (minf end point))
	    (from-point (maxf beg point)))
      (if with-properties
	(buffer-substring beg end)
	(buffer-substring-no-properties beg end)))))
;;(current-line-as-string :to-point t :from-point nil)

(defun current-line-as-string (&rest args)
  (warn "Deprecated. Use LINE-STRING instead.")
  (apply #'line-string args))

(cl-defun column-at (&optional (point (point)))
  (save-excursion
    (goto-char point)
    (current-column)))

(cl-defun last-column (&optional (point (point)))
  (save-excursion
    (goto-char point)
    (eol)
    (current-column)))

(defun blank-line-p ()
  "Returns non-nil iff current line is an empty string."
  (save-excursion (= (bol) (eol))))
;;(blank-line-p)

(defun number-of-lines ()
  (save-excursion
    (eob) (line-number-at-pos)))
;;(number-of-lines)

(defun flatten-paragraph ()
  "Remove all newline characters from current paragraph"
  (let ((fill-column (- (reduce #'- (paragraph-region))))
	(fill-paragraph-function nil))
    (fill-paragraph)))

;;;; time paragraphs (used by LS-notes and arbeidslog)
;;;; time paragraphs:
(defun in-time-paragraph-p ()
  "Returns non-nil iff point is in the Octave documentation.
Currently only the documentation of the main function is
supported. To implement the general version we need to know if we
are between functions."
  (save-excursion
    (string-match (format "^%s " (iso-time-regexp)) (paragraph-string))))

(defun fill-time-paragraph (&optional justify region)
  "A fill-paragraph-function suited for time paragraphs.
See arbeidslog for an example of a time paragraph"
  (interactive)
  (if (in-time-paragraph-p)
    (progn
      (flatten-paragraph)
      (let ((fill-prefix "      ")
	    (fill-paragraph-function nil))
	(fill-paragraph justify region)))
    (fill-paragraph justify region)))

(provide 'mb-utils-buffer)
