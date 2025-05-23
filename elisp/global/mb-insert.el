(require 'mb-calendar)
(require 'mb-utils-strings)

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
  (and (cl-member ensure-space '(:before :around) :test #'equal)
       (not (bobp))
       (memq (char-syntax (preceding-char)) '(?w ?_ ?\) ))
       (insert " "))
  (insert left)
  (save-excursion
    (or (eq arg 0) (forward-sexp arg))
    (insert right)
    (and (cl-member ensure-space '(:after :around))
	 (not (eobp))
	 (memq (char-syntax (following-char)) '(?w ?_ ?\( ))
	 (insert " "))))
;;(insert-parentheses* 1 :ensure-space :around)

(cl-defun insert-date (prefix)
  "Inserts today's date string at point."
  (interactive "P")
  (insert
   (if prefix
     (iso-date-and-time
      :with-seconds (and (numberp prefix)
			 (= prefix 3))) 
     (iso-date))))
;;(insert-date 3)

(cl-defun insert-time (prefix)
  "Inserts today's date string at point."
  (interactive "P")
  (insert (iso-time :with-seconds (and (numberp prefix) 
				       (= prefix 3)))))

(cl-defun insert-date-and-time (prefix)
  "Inserts today's date string at point."
  (interactive)
  (insert (iso-date-and-time :with-seconds (and (numberp prefix) 
						(= prefix 3)))))

(cl-defun insert-full-date ()
  "Inserts today's date string at point."
  (interactive)
  (insert (full-date)))

(cl-defun insert-todo-item ()
  "Inserts a new item for a todo list."
  (interactive)
  (beginning-of-line 1)
  (insertf "* %s \n" (short-date))
  (end-of-line 0))

(cl-defun iso-to-short-date-at-point ()
  "Converts iso-date at point to short date."
  (interactive)
  (apply #'overwrite-region
    (short-date (date-at-point t)) (thing-at-point-bounds-of-date t)))

;; manipulation
(cl-defun kill-sexp* (n)
  "Kills sexp at point and returns it as a string. Probably better to
use kill-ring directly, but not now."
  (backward-kill-sexp n)
  (first kill-ring))
;;(get-text-property 0 1)

(cl-defun insert-quotes (n)
  "Inserts quotes \(`'\) at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\` :right ?\'))

(cl-defun insert-single-quotes (n)
  "Inserts pair of single quotes at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?' :right ?'))

(cl-defun insert-double-quotes (n)
  "Inserts pair of double quotes at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\" :right ?\"))

(cl-defun insert-asterisks (n)
  "Inserts pair of asterisks at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?* :right ?*))

(cl-defun insert-guillemets (n)
  "Inserts �� at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\� :right ?\�))

(cl-defun insert-square-brackets (n)
  "Inserts \[\] at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?\[ :right ?\] :ensure-space :after))

(cl-defun insert-curly-brackets (n)
  "Inserts \{\} at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?{ :right ?}))

(cl-defun insert-angle-brackets (n)
  "Inserts \<\> at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?< :right ?>))

(cl-defun insert-dollar-brackets (n)
  "Inserts $$ at point. See `insert-parentheses' for N."
  (interactive "P")
  (insert-parentheses* n :left ?$ :right ?$))

(cl-defun insert-xml-tag (n tag)
  "Inserts <TAG></TAG> around point."
  (interactive "P\nsTag: ")
  (insert-parentheses* n :left (format "<%s>" tag) :right (format "</%s>" tag)))

(cl-defun insert-skip-quote (n)
  "Inserts \[...\] at point. Used to show that a part of quoted text
is skipped. See `insert-parentheses' for N."
  (interactive "P")
  (insert "[...]"))

(cl-defun insert-post-scriptum ()
  "Inserts post scriptum delimiters at point."
  (interactive)
  (insert "\nP.S. \nD.S.")
  (backward-char 5))

(defmacro insert-n (x)
  "Should provide doc, but it is more difficult than you would think!
X should evaluate to a string designator."
  (with-gensyms (string-designator)
    `(let ((,string-designator ,x))
       (lambda (&optional n)
	 (interactive "P")
	 (dotimes (i (or n 1))
	   (insert (sstring ,string-designator)))))))
;;(funcall (insert-n '�) 2)
;;(funcall (insert-n "qwe") 2)

(cl-defun insert-symbol-sup1 (n)
  "Inserts N superscript 1s (�) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-sup2 (n)
  "Inserts N superscript 2s (�) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-sup3 (n)
  "Inserts N superscript 3s (�) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-deg (n)
  "Inserts N degree signs (�) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-ordm (n)
  "Inserts N masculine ordinals (�) at point.
This function is obsolete. Use C-x 8 _ o instead."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-ordf (n)
  "Inserts N feminine ordinals (�) at point.
This function is obsolete. Use C-x 8 _ a instead."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-macr (n)
  "Inserts N spacing macrons at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-symbol-bullet (n)
  "Inserts N round filled bullets at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-german-double-s (n)
  "Inserts German double s at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-c-cedilla (n)
  "Inserts c with cedilla (�) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))

(cl-defun insert-1/2 (n)
  "Inserts c with cedilla (�) at point."
  (interactive "P")
  (dotimes (i (or n 1)) (insert ?�)))


(defvar mb-default-underline-pattern "=")

(cl-defun insert-underline (pattern)
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

(cl-defun insert-prefix-region (beg end &optional prefix)
  "Inserts PREFIX at beginning of each line in region. Default is
\"> \"."
  (interactive "*r")
  (while (re-search-forward "\n")))

(defmacro insert-object (obj)
  `(insert (format "%s" ,obj)))

(cl-defun ins (&rest objs)
  (dolist (o objs) (insert-object o)))

(cl-defun insert-expression (expression)
  "Inserts the evaluation of EXPRESSION at point."
  (interactive (list (eval-minibuffer "Insert expression: ")))
  (ins expression))

(cl-defun insert-provide ()
  (interactive)
  (save-excursion
    (eob)
    (delete-blank-lines)
    (insert (format "\n(provide '%s)"
	      (file-name-sans-extension (buffer-name))))))
;;(insert-provide)

(cl-defun insert-alphabet (n)
  "Insert the alphabet.
With a prefix you can select another language than English.
Absolut value of prefix defines the language. The sign defines
the orientation: if it is negative, the alphabet is inserted
horizontally. Else, it is interted vertically."
  (interactive "p")
  (let ((a (cl-case (abs n)
	     (2 (concat (a-b ?A ?Z) '(?� ?� ?�)))
	     (3 (concat (a-b ?A ?Z) '(?� ?� ?�)))
	     (t (a-b ?A ?Z)))))
    (insert (apply #'string (if (minusp n) a (infix-list ?\n a))))))
;;(insert-alphabet -1)

(provide 'mb-insert)
