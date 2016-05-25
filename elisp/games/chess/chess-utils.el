(defun symbol-to-char (symbol)
  (char (symbol-name symbol) 0))

(defun chars-to-string (chars)
  (apply #'string chars))

(defun* string* (chars &key (in nil))
  (let ((chars* (loop for char in chars
		      for i from 0
		      collect char
		      if in collect (if (functionp in) 
				      (funcall in i)
				      in))))
    (apply #'string 
	   (if in
	     (nbutlast chars*)
	     chars*))))
;;(string* (first board-matrix-start))

(defun* count-lines-buffer (&optional (buffer (current-buffer)))
  "Returns the number of lines in BUFFER"
  (with-buffer buffer
    (count-lines (point-min) (point-max))))
;;(count-lines-buffer)

(defun* line-length (&optional (line (line-number-at-pos)) (buffer (current-buffer)))
  "Returns the lenght of LINE in BUFFER"
  (with-buffer buffer
    (save-excursion 
      ;;exploits the fact that move-to-column returns the final column
      (move-to-column most-positive-fixnum))))
;;(line-length)

(defun* negative-p (number &optional (strictly-p t)) 
  "Returns t iff NUMBER is strictly less than zero.
If STRICTLY-P is nil, then the function returns t also when
NUMBER is zero."
  (if strictly-p
    (< number 0)
    (<= number 0)))
;;(mapcar (bind #'negative-p nil) '(-1 -0.1 -0.0 -0 0.0 0 0.1 1))

(defun* positive-p (number &optional (strictly-p t)) 
  "Returns t iff NUMBER is greater than zero.
If STRICTLY-P is nil, then the function returns t also when
NUMBER is zero."
  (if strictly-p
    (> number 0)
    (>= number 0)))
;;(mapcar (bind #'positive-p t) '(-1 -0.1 -0.0 -0 0.0 0 0.1 1))

(defun move-to-line (line &optional force)
  (goto-line line)
  (when (and force (< (line-number-at-pos) line))
    (insert (make-string (- line (line-number-at-pos)) 10))))
;;(move-to-line 85)

(defun column-line (&optional point)
  (if point
    (save-excursion
      (goto-char point)
      (column-line))
    (list (current-column) (line-number-at-pos))))
;;(column-line 1923)
(defun ecl-line (column-line) (second column-line))
(defun ecl-column (column-line) (first column-line))

(defun ecl-add (column-line1 column-line2) 
  (mapcar* #'+ column-line1 column-line2))
;;(ecl-add '(2 3) '(10 7))

(defun* goto-char* (column-line &optional (fill-line ""))
  "Same as `goto-char', but works also if COLUMN-LINE exceeds
buffer size. For optional arguments, see `goto-line*' and
`goto-char*'."
  (move-to-line (ecl-line column-line) t)
  (move-to-column (ecl-column column-line) t)
  (point))
;;(goto-char* '(32 83))

(defun overwrite (printable)
  "Overwrites at point with PRINTABLE"
  (let ((s (sstring printable)))
    (insert s)
    (kill-region (point) (+ (point) (length s)))))

(defun* insert-at (printable column-line &optional overwrite-p (buffer (current-buffer)))
  "Inserts PRINTABLE at LINE's COLUMNth position in BUFFER. If
OVERWRITE-P then the method overwrites."
  (with-buffer buffer 
    (goto-char* column-line)
    (if overwrite-p 
      (overwrite printable)
      (insert printable))))
;;(insert-at "qwe" (list 3 94) t)

(provide 'chess-utils)
