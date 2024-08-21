(cl-defun empty-buffer-p (&optional (buffer (current-buffer)))
  (with-buffer buffer
    (empty-string-p (buffer-string))))
;;(empty-buffer-p)

(defun buffer-major-mode (buffer-or-name)
  "Return the major mode of the buffer designated by BUFFER-OR-NAME.
The result type is SYMBOL."
  (buffer-local-value 'major-mode (get-buffer buffer-or-name)))

(defun major-mode-p (mode buffer-or-name)
  "Return non nil if the major mode of BUFFER-OR-NAME is MODE."
  (eql (buffer-major-mode buffer-or-name) mode))

(cl-defun buffer-file-name-nondirectory (&optional (buffer (current-buffer)))
  (file-name-nondirectory (buffer-file-name buffer)))
;;(buffer-file-name-nondirectory)

(cl-defun buffer-directory (&optional (buffer (current-buffer)))
  (file-name-directory (buffer-file-name buffer)))
;;(buffer-directory)
;;(def-edebug-spec buffer-directory (&optional (symbolp form)))

(defun region-beginning* (&optional force)
  "See `region-beginning' and `mark'"
  (min (point) (mark force)))

(defun region-end* (&optional force)
  "See `region-end' and `mark'"
  (max (point) (mark force)))
;;(region-end*)

(defun region (&optional force)
  (list (region-beginning*) (region-end*)))
;;(region)

(defun mark-region (beg-or-region &optional end)
  (if (not end)
    (apply #'mark-region beg-or-region)
    (push-mark beg-or-region nil t)
    (goto-char end)))

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
  (let ((a (cl-case unit
	     (:point (rpoint start))
	     (:line (buffer-line-point start))
	     (:paragraph (buffer-paragraph-point start))))
	(b (if end
	     (cl-case unit
	       (:point (rpoint end))
	       (:line (buffer-line-point end))
	       (:paragraph (buffer-paragraph-point end)))
	     (point-max))))
    (if with-properties
      (buffer-substring a b)
      (buffer-substring-no-properties a b))))
;;(buffer-substring* :start (point) :length 1 :unit :line)
;;(point-min)

(cl-defun insert-at (thing &optional (point (point)) (n 1))
  "`insert's THING at POINT."
  (save-excursion
    (goto-char point)
    (cl-loop repeat n do (insert thing))))
;;(insert-at "qwe" (point) 3)

(defun overwrite-region (string beg end &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (kill-region beg end)
    (insert-at string beg)))

(cl-defun overwrite-line (line linum &optional (buffer (current-buffer)))
  (cl-destructuring-bind (beg end) (line-region linum buffer)
    (overwrite-region line beg end buffer)))
;;(overwrite-line "newline" 2 "*scratch*")
;;(overwrite-line "newline" 2)

(defun next-line-point () "Return position of point after nextline"
  (save-excursion
    (next-line 1)
    (point)))


;;; Beginning / end of helper functions 
(cl-defun bo-thing (thing &optional n)
  "Move to the beginning of the current THING and return point."
  (awhen (bounds-of-thing-at-point thing)
    (goto-char (car it))
    (when n (forward-thing thing (- 1 n)))
    (point)))

(defun eo-thing (thing &optional n)
  "Move to the end of the current word and return POINT."
  (awhen (bounds-of-thing-at-point thing)
    (goto-char (cdr it))
    (when n (forward-thing thing (1- n)))
    (point)))


;;; Sexp stuff
;;; TODO rename these as bof, eof etc, and reserve bos, eos etc to symbols
;;; because sexp and form is indeed the same
(defun bos (&optional n)
  "Move POINT back N sexps and return point"
  (bo-thing 'sexp n))

(defun eos (&optional n)
  "Move POINT forward N sexps and return point"
  (eo-thing 'sexp n))

(defun bos* (&optional n)
  "Return the POINT at the beginning of the Nth sexp before current point."
  (save-excursion (bos n)))

(defun eos* (&optional n)
  "Return the POINT at the beginning of the Nth sexp before current point."
  (save-excursion (eos n)))

(defun sexp-region () (list (bos* 1) (eos* 1)))

(defun backward-sexp* (&optional n)
  (backward-sexp n) (point))

(defun c-last-sexp-region ()
  "Return the region of the sexp preceeding point.
This function assumes a C-like syntax. In particular, it assumes
that there is no space between identifiers and delimiters"
  (let ((end (point)))
    (save-excursion
      (cl-loop for res = (ignore-errors (backward-sexp* 1))
	       until (or (null res)
			 (member (char-before) '(9 10 32))))
      (list (point) end))))

(cl-defun last-sexp-region (&optional (n 1))
  (cl-case major-mode
    ((js-mode mbscilab-mode) (c-last-sexp-region))
    (otherwise (save-excursion (list (bos n) (eos n))))))

;;; Region stuff
(defalias 'region-string 'buffer-substring-no-properties)
;;(region-string 1 100)

(cl-defun region-replace-raw (new-string region &optional (point (car region)))
  "Replace the content of current buffer's REGION with NEW-STRING.
The new NEW-STRING is inserted at the beginning of REGION, or at
the optonal POINT."
  (save-excursion
    (goto-char point)
    (apply #'delete-region region)
    (insert new-string)))

(cl-defun region-replace (format &optional (region (region)) (point (car region)))
  "Replace the content of REGION with the string defined by FORMAT.
REGION is a pair of points (START END) belonging to BUFFER, which
is the current buffer by default. This region is replaced by the
string FORMAT after each substring \"%s\" has been substuted by
the content of REGION.

For optional argument point, see `region-replace-raw'."
  (region-replace-raw
   (replace-regexp-in-string "%s" (apply #'region-string region) format)
   region point))
;;(region-replace "(%s)") test reg(i){i}on

(defun region-lines (beg end)
  "Returns a list of strings, each string being line in region"
  (interactive "r")
  (string-to-lines (region-string beg end)))

(defun region-as-line-strings (beg end)
  (interactive "r")
  (warn "Deprecated. Use REGION-LINES instead."))

;;; Buffer stuff
(defsubst bob ()
  "Move POINT to beginning of buffer and return its value"
  (goto-char (point-min)))

(defsubst eob (&optional ensure-newline-p)
  "Move POINT to end of buffer and return its value.
If optional ENSURE-NEWLINE-P is not nil, then insert a newline at the
end of buffer if not already present."
  (goto-char (point-max))
  (when (and ensure-newline-p (plusp (current-column)))
    (newline))
  (point))
;;(save-excursion (eob))

(cl-defun buffer-region (&optional (buffer (current-buffer)))
  (with-buffer buffer (save-excursion (list (bob) (eob)))))
;;(buffer-region)

(cl-defun buffer-lines (&optional (buffer (current-buffer)))
  (with-buffer buffer (save-excursion (region-lines (bob) (eob)))))
;;(buffer-lines)

(cl-defun buffer-string* (&optional (buffer (current-buffer)))
  (with-buffer buffer (buffer-string)))
;;(buffer-lines)

;;; Paragraph stuff
(defsubst bop ()
  "Move POINT to beginning of paragraph and return its value"
  (backward-paragraph 1)
  (when (blank-line-p)
    (forward-char 1))
  (point))
;;(bop)

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

(defun current-paragraph-as-string ()
  (warn "Deprecated. Use PARAGRAPH-STRING instead.")
  (paragraph-string))

;;; Line stuff
;; NOTE: To calculate the line number from POINT, use line-number-at-pos

(cl-defun bol (&key linum point (offset 0) restrict-to-current-field)
  "Move point to the beginning of the current line and return its value.
If RESTRICT-TO-CURRENT-FIELD is true, the function constrains
point to the current field, see `beginning-of-line'"
  (let ((inhibit-field-text-motion (not restrict-to-current-field)))
    (if linum
      (goto-line linum)
      (if point (goto-char point)))
    (beginning-of-line (1+ offset))
    (point)))
;;(bol :point 9867)

(cl-defun eol (&key linum point (offset 0) restrict-to-current-field)
  "Move point to the end of the current line and return its value.
For RESTRICT-TO-CURRENT-FIELD, see `bol'."
  (let ((inhibit-field-text-motion (not restrict-to-current-field)))
    (if linum
      (goto-line linum)
      (if point (goto-char point)))
    (end-of-line (1+ offset))
    (point)))
;;(eol :point 10000)
;;(save-excursion (list (bol) (eol) (bob) (eob)))

(defun bol* (&rest args)
  "Return the same as `bol' without moving the POINT.
Note: if you are not using any of the keyword arguments, see
`bol', you should stick to the standard function
`line-beginning-position' instead."
  (when (null args)
    (warn "bol* called without keyword arguments from function: %s"
	  ;; Note that 6 is carefully calculated from the current form
	  ;; hierarchy
	  (second (backtrace-frame 6))))
  (save-excursion (apply #'bol args)))

(defun eol* (&rest args)
  "Return the same as `eol' without moving the POINT.
Note: if you are not using any of the keyword arguments, see
`eol', you should stick to the standard function
`line-end-position' instead."
  (when (null args)
    (warn "eol* called without keyword arguments from function: %s"
	  ;; Note that 6 is carefully calculated from the current form
	  ;; hierarchy
	  (second (backtrace-frame 6))))
  (save-excursion (apply #'eol args)))

(cl-defun line-region (&optional linums (buffer (current-buffer)))
  "Returns the region covering the current line in BUFFER.
If LINUMS is specified it returns the region covering those
lines."
  (with-buffer buffer
    (list (bol :linum (and linums (reduce #'min (listify linums))))
	  (eol :linum (and linums (reduce #'max (listify linums)))))))
;;(line-region)

(cl-defun line-string (&key to-point from-point
			    restrict-to-current-field
			    with-properties (point (point)))
  "Return a string of the line containing point.
If TO-POINT is true, only the part of line before `point' is
returned. If FROM-POINT is true, only the part of line after
`point' is returned. If WITH-PROPERTIES is true, properties are
included in the returned string. For RESTRICT-TO-CURRENT-FIELD,
see `bol'"
  (cl-assert (nand to-point from-point)
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

;;; Defun stuff 
(defun bod (&optional n)
  "Move to beginning of the current defun and return POINT."
  ;; Can't use bo-thing since bounds-of-thing-at-point fails when
  ;; defun is not indented according to common style!
  (condition-case nil (forward-char 1) (error nil))
  (beginning-of-defun n)
  (point))
;;(bod)

(defun eod (&optional n)
  "Move to end of the current defun and return POINT."
  ;; See implementation note in `bod'.
  (condition-case nil (backward-char 1) (error nil))
  (end-of-defun n)
  (backward-char 1)
  (point))
;;(eod)

(defun bod* (&optional n)
  "Return the POINT at the beginning of the current defun."
  (save-excursion (bod)))
;;(bod*)

(defun eod* (&optional n)
  "Return the POINT at the end of the current defun."
  (save-excursion (eod)))
;;(eod*)

(defun defun-region ()
  "Return the region of the current defun.
The return value is a pair of points \(START END\)."
  (list (bod*) (eod*)))

(defun defun-string ()
  (buffer-substring-no-properties (bod*) (eod*)))

;;; form
(defun bof (&optional n)
  "Move to the beginning of the current form and return POINT."
  (bo-thing 'list n))

(defun eof (&optional n)
  "Move to the end of the current form and return POINT."
  (eo-thing 'list n))
;;(eof)

(defun bof* (&optional n)
  "Return the POINT at the beginning of the current form."
  (save-excursion (bof)))
;;(bof*)

(defun eof* (&optional n)
  "Return the POINT at the end of the current form."
  (save-excursion (eof)))
;;(eof*)

;;; word
(defun bow (&optional n)
  "Move to the beginning of the current word and return POINT."
  (bo-thing 'word n))

(defun eow (&optional n)
  "Move to the end of the current word and return POINT."
    (eo-thing 'word n))

(defun bow* (&optional n)
  "Return the POINT at the beginning of the current word."
  (save-excursion (bow n)))
;;(bow*)

(defun eow* (&optional n)
  "Return the POINT at the end of the current defun."
  (save-excursion (eow n)))
;;(eow* 3)

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

;;;; time paragraphs (used arbeidslog, for instance):
(defconst time-paragraph-start (format "^%s " (iso-time-regexp)))

(defun in-time-paragraph-p ()
  "Returns non-nil iff point is in a time paragraph."
  (save-excursion
    (string-match time-paragraph-start (paragraph-string))))

(defun fill-time-paragraph (&optional justify region)
  "A fill-paragraph-function suited for time paragraphs.
See arbeidslog for an example of a time paragraph"
  (interactive)
  (if (in-time-paragraph-p)
    (let ((paragraph-start time-paragraph-start))
      (flatten-paragraph)
      (let ((fill-prefix "      ")
	    (fill-paragraph-function nil))
	(fill-paragraph justify region)))
    (fill-paragraph justify region)))


;; A few helpers
(defvar *sexp-statt-word* nil)

(defun count-w-or-s (beg end)
  (if *sexp-statt-word*
    (count-sexps-region beg end)
    (count-words beg end)))

(defun transpose-w-or-s (n)
  (if *sexp-statt-word*
    (transpose-sexps n)
    (transpose-words n)))

(defun forward-w-or-s (n)
  (if *sexp-statt-word*
    (forward-sexp n)
    (forward-word n)))

(cl-defun rotate-words-1 (beg end &optional (n 1))
  "Rotate words in region from BEG to END N times.
N is optional, and is 1 by default."
  (let ((nwords (count-w-or-s beg end)))
    (when (plusp nwords)
      (save-excursion
	(cl-loop repeat (mod (- n) nwords)
		 do (goto-char beg)
		 do (forward-w-or-s 1)
		 do (transpose-w-or-s (1- nwords)))))))

(cl-defun rotate-words (&optional (n 1))
  "Rotate words in active region.
If region is not active this function is equivalent with
`transpose-words'."
  (interactive "^p")
  (if (use-region-p)
    (rotate-words-1 (region-beginning) (region-end) n)
    (transpose-w-or-s n)))

(cl-defun rotate-sexps (&optional (n 1))
  "Rotate symbol in active region.
If region is not active this function is equivalent with
`transpose-sexps'."
  (interactive "^p")
  (let ((*sexp-statt-word* t))
    (rotate-words n)))

(defun reverse-words-1 (beg end)
  "Reverse words in region from BEG to END.
Helper function for `reverse-words'."
  (let ((n (count-w-or-s beg end)))
    (when (plusp n)
      (save-excursion
	(goto-char beg)
	(cl-loop for i from (1- n) downto 1
		 do (rotate-words-1 (point) end 1)
		 do (forward-w-or-s 1))))))

(cl-defun reverse-words (&optional (n 1))
  "Reverse words in active region.
If region is not active this function is equivalent with
`transpose-words'."
  (interactive "^p")
  (if (use-region-p)
    (reverse-words-1 (region-beginning) (region-end))
    (transpose-w-or-s n)))

(cl-defun reverse-sexps (&optional (n 1))
  "Reverse sexps in active region.
If region is not active this function is equivalent with
`transpose-sexps'."
  (interactive "^p")
  (let ((*sexp-statt-word* t))
    (reverse-words n)))

(define-key global-map "\M-t" #'reverse-words)

(defalias 'reverse-lines #'reverse-region)

(defun toggle-window-split ()
  "Toggle between horizontal and vertical split of two windows.
Copied from http://stackoverflow.com/questions/14881020/\
emacs-shortcut-to-switch-from-a-horizontal-split-to-a-vertical-split-in-one-move"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(defun just-one-blank-line (&optional n)
  "Delete all blank lines above and below current leaving one blank line
\(or N blank lines\).
Should this method be interactive?"
  (newline 3)
  (previous-line 1)
  (delete-blank-lines)
  (when (and n (numberp n) (plusp n))
    (newline (* 2 (1- n)))
    (previous-line (1- n))))

(defmacro with-other-window (&rest body)
  "Eval BODY in the other window's buffer."
  `(prog2
       (other-window 1)
       (progn ,@body)
     (other-window 1)))

(defun comment-region* (beg end &optional arg)
  (interactive "*r\nP")
  (if (use-region-p)
    (comment-region beg end arg)
    (comment-region (line-beginning-position) (line-end-position) arg)))

(defun uncomment-region* (beg end &optional arg)
  (interactive "*r\nP")
  (if (use-region-p)
    (uncomment-region beg end arg)
    (uncomment-region (line-beginning-position) (line-end-position) arg)))

;;; Smart comment stuff
(defun commented-string-p (string)
  "Returns nil iff current line is not commented"
  (warn "commented-string-p is not supported!")
  (string-match* "^[[:space:]]*;" string))

(defun commented-line-p ()
  "Returns nil iff current line is not commented"
  (comment-only-p (line-beginning-position) (line-end-position)))
;;(commented-line-p)

(defun toggle-comment-line (&optional arg)
  "Toggle comment on current line in buffer."
  (cl-destructuring-bind (beg end) (line-region)
    (if (commented-line-p)
     (uncomment-region beg end arg)
     (comment-region beg end arg))))

(defun toggle-comment-region (beg end &optional arg)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (toggle-comment-line arg)
      (forward-line 1))))

(defun toggle-comment-region* (beg end &optional arg)
  (interactive "*r\nP")
  (if (use-region-p)
    (toggle-comment-region beg end arg)
    (toggle-comment-region (line-beginning-position) (line-end-position) arg)))

;;;; Save as for images etc
(defun save-as-1 (filename)
  "Write current buffer file to FILENAME, and rename buffer.
It assumes that there will be no write conflicts."
  (copy-file (buffer-file-name) new)
  (set-visited-file-name new)
  (set-buffer-modified-p nil))

(defun save-as ()
  "Generalization of `write-to': also handles png-files etc."
  (interactive)
  (let ((new (read-file-name "Save as: ")))
    (if (file-exists-p new)
      (when (yes-or-no-p
	     (format "File %s already exists. Do you really want to overwrite? "
	      new))
	(delete-file new)
	(save-as-1 new))
      (save-as-1 new))))

(defmacro within-temp-file (&rest body)
  "Create a temporary file and evaluate BODY there.
Afterwards delete the file."
  `(let ((file (make-temp-file "qwe")))
     (with-file file
       (message "buffer: %s" ,(buffer-directory))
       (prog1 (progn ,@body)
	 (delete-file file)))))
;;(within-temp-file (+ 2 2))
(def-edebug-spec within-temp-file progn)

(defun string-to-clipboard (string &optional hide-message)
  "Copy STRING to clipboard.
If HIDE-MESSAGE is NIL, the default, a message informing what was
copied is displayed."
  (gui-set-selection 'CLIPBOARD string)
  (unless hide-message
    (message "Copied string '%s' to clipboard" string)))

(defun buffer-file-name-to-clipboard ()
  (interactive)
  (string-to-clipboard (buffer-file-name)))

(defun sort-lines-with-comments ()
  (interactive)
  (let ((new-lines (sort-strings-with-comments (buffer-lines) comment-start)))
    (apply #'delete-region (buffer-region))
    (insert (concat* new-lines :in "\n"))))
;;(sort-lines-with-comments)

(defun transpose-string (string)
  "Insert a newline between each character in STRING"
  (coerce (nbutlast (zip (coerce string 'list) 10)) 'string))

(defun insert-alphabet (n)
  "Insert alphabet at point"
  (interactive "P")
  (unless n (setf n 0))
  (let ((string (alphabet (if (zerop (logand n 1)) :en :no))))
    (unless (zerop (logand n 2))
      (setf string (transpose-string string)))
    (unless (zerop (logand n 4))
      (setf string (upcase string)))
    (insert string)))

(provide 'mb-utils-buffer)
