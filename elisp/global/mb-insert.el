(cl-defun insert-parentheses* (arg &key (left ?\() (right ?\))
				   (ensure-space :around))
  "Extension of insert-parentheses. Enclose following ARG sexps in
parentheses. Leave point after open-paren. A negative ARG encloses the
preceding ARG sexps instead. No argument is equivalent to zero: just
insert `()' and leave point between. If :ENSURE-SPACE is :AROUND :BEFORE
or :AFTER this command also inserts a space before and after, before
only, or left only depending on the surrounding characters. If
:ENSURE-SPACE is :NONE, no such insertion is performed."
  (unless arg (setq arg 0))
  (cond ((> arg 0) (skip-chars-forward " \t"))
	((< arg 0) (forward-sexp arg) (setq arg (- arg))))
  (and (member* ensure-space '(:before :around) :test #'equal)
       (not (bobp))
       (memq (char-syntax (preceding-char)) '(?w ?_ ?\) ))
       (insert " "))
  (insert left)
  (save-excursion
    (or (eq arg 0) (forward-sexp arg))
    (insert right)
    (and (member* ensure-space '(:after :around))
	 (not (eobp))
	 (memq (char-syntax (following-char)) '(?w ?_ ?\( ))
	 (insert " "))))
;;(insert-parentheses* 1 :ensure-space :around)

(require 'mb-calendar)

(defun insert-date (prefix)
  "Inserts today's date string at point."
  (interactive "P")
  (insert
   (if prefix
     (iso-date-and-time
      :with-seconds (and (numberp prefix)
			 (= prefix 3))) 
     (iso-date))))
;;(insert-date 3)

(defun insert-time (prefix)
  "Inserts today's date string at point."
  (interactive "P")
  (insert (iso-time :with-seconds (and (numberp prefix) 
				       (= prefix 3)))))

(defun insert-date-and-time ()
  "Inserts today's date string at point."
  (interactive)
  (insert (iso-date-and-time)))

(defun insert-full-date ()
  "Inserts today's date string at point."
  (interactive)
  (insert (full-date)))

(defun insert-todo-item ()
  "Inserts a new item for a todo list."
  (interactive)
  (beginning-of-line 1)
  (insertf "* %s \n" (short-date))
  (end-of-line 0))

(defun iso-to-short-date-at-point ()
  "Converts iso-date at point to short date."
  (interactive)
  (let ((date (decode-iso-date (date-at-point)))
	(bounds (thing-at-point-bounds-of-date)))
    (delete-region (car bounds) (cdr bounds))
    (insert (short-date date))))

;; manipulation
(defun kill-sexp* (n)
  "Kills sexp at point and returns it as a string. Probably better to
use kill-ring directly, but not now."
  (backward-kill-sexp n)
  (first kill-ring))
;;(get-text-property 0 1)

(defun insert-quotes (n)
  "Inserts quotes \(`'\) at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\` :right ?\'))

(defun insert-single-quotes (n)
  "Inserts pair of single quotes at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?' :right ?'))

(defun insert-double-quotes (n)
  "Inserts pair of double quotes at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\" :right ?\"))

(defun insert-asterisks (n)
  "Inserts pair of asterisks at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?* :right ?*))

(defun insert-guillemets (n)
  "Inserts «» at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\« :right ?\»))

(defun insert-square-brackets (n)
  "Inserts \[\] at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\[ :right ?\] :ensure-space :after))

(defun insert-curly-brackets (n)
  "Inserts \{\} at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?{ :right ?}))

(defun insert-angle-brackets (n)
  "Inserts \<\> at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?< :right ?>))

(defun insert-dollar-brackets (n)
  "Inserts $$ at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?$ :right ?$))

(defun insert-xml-tag (n tag)
  "Inserts <TAG></TAG> around point."
  (interactive "P\nsTag: ")
  (insert-parentheses* n :left (format "<%s>" tag) :right (format "</%s>" tag)))

(defun insert-skip-quote (n)
  "Inserts \[...\] at point. Used to show that a part of quoted text
is skipped. See `insert-parentheses' for N."
  (interactive "P")
  (insert "[...]"))

(defun insert-post-scriptum ()
  "Inserts post scriptum delimiters at point."
  (interactive)
  (insert "\nP.S. \nD.S.")
  (backward-char 5))

(defmacro insert-n (x)
  "Should provide doc, but it is more difficult than you would think!
X should evaluate to a string designator."
  (with-gensyms (string-designator)
    `(lexical-let ((,string-designator ,x))
       (lambda (&optional n)
	 (interactive "P")
	 (dotimes (i (or n 1))
	   (insert (sstring ,string-designator)))))))
;;(funcall (insert-n '¹) 2)
;;(funcall (insert-n "qwe") 2)

(defun insert-symbol-sup1 (n) "Inserts N superscript 1s (¹) at point." (insert-n )
       (interactive "P")
       (dotimes (i (or n 1)) (insert ?¹)))

(defun insert-symbol-sup2 (n)
  "Inserts N superscript 2s (²) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?²)))

(defun insert-symbol-sup3 (n)
  "Inserts N superscript 3s (³) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?³)))

(defun insert-symbol-deg (n)
  "Inserts N degree signs (°) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?°)))

(defun insert-symbol-ordm (n)
  "Inserts N masculine ordinals (º) at point.
This function is obsolete. Use C-x 8 _ o instead."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?º)))

(defun insert-symbol-ordf (n)
  "Inserts N feminine ordinals (ª) at point.
This function is obsolete. Use C-x 8 _ a instead."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?ª)))

(defun insert-symbol-macr (n)
  "Inserts N spacing macrons at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?¯)))

(defun insert-symbol-bullet (n)
  "Inserts N round filled bullets at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?·)))

(defun insert-german-double-s (n)
  "Inserts German double s at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?ß)))

(defun insert-c-cedilla (n)
  "Inserts c with cedilla (ç) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?ç)))

(defvar mb-default-underline-pattern "=")

(require 'mb-utils-strings)

(defun insert-underline (pattern)
  "Underlines the current line with a string of multiple concatenated
PATTERNs. The underline is inserted imediately after current line and
has the same length as current line."
  (interactive "*sUnderline pattern: " )
  (let ((len (last-column))
	(line pattern))
    (if (string= line "") (setq line mb-default-underline-pattern))
    (while (< (length line) len)
      (setq line (concat line line)))
    (newline)
    (insert (substring line 0 len))))
;;(insert-underline "hallo paa do")

(defun insert-prefix-region (beg end &optional prefix)
  "Inserts PREFIX at beginning of each line in region. Default is
\"> \"."
  (interactive "*r")
  (while (re-search-forward "\n")))

(defmacro insert-object (obj)
  `(insert (format "%s" ,obj)))

(defun ins (&rest objs)
  (dolist (o objs) (insert-object o)))

(defun insert-expression (expression)
  "Inserts the evaluation of EXPRESSION at point."
  (interactive (list (eval-minibuffer "Insert expression: ")))
  (ins expression))

(provide 'mb-insert)
